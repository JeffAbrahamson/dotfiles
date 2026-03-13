#!/usr/bin/env python3
"""
Estimate time remaining until a reserve reaches zero using a state-space model
with a random-walk consumption rate + Monte-Carlo hitting-time simulation.

Input file format:
    YYYY-MM-DD <whitespace> quantity
Dates may be unsorted. Quantity is an integer (may appear as float like 37.0).

Example:
    2025-03-01   120
    2025-03-09   112
    2025-03-15   100
    2025-03-30   98
    2025-04-10   98
    2025-04-22   90

Usage:
    tsd-time-to-empty.py -f data.txt
"""

import argparse
import math
import sys
from dataclasses import dataclass
from datetime import date, datetime
from typing import List, Optional, Tuple

import numpy as np

DATE_FMT = "%Y-%m-%d"


@dataclass
class Options:
    path: str
    sigma_r: float
    sigma_q: float
    sigma_z: float
    nsims: int
    dt_forward: float
    max_days: float
    seed: Optional[int]
    allow_negative_rate: bool
    min_rate: float
    bins: int
    quantiles: Tuple[float, ...]
    drop_same_day_duplicates: bool


def parse_args() -> Options:
    """Parse command-line arguments and return a validated Options object."""
    p = argparse.ArgumentParser(
        description="State-space random-walk rate + Monte-Carlo time-to-empty estimator"
    )
    p.add_argument(
        "-f", "--file", required=True, help="Input file path (date, quantity)"
    )
    p.add_argument(
        "--sigma-r",
        type=float,
        default=0.50,
        help="Rate random-walk std per sqrt(day). Larger -> faster rate drift (default: 0.50)",
    )
    p.add_argument(
        "--sigma-q",
        type=float,
        default=0.25,
        help="Process noise std on quantity per sqrt(day) (default: 0.25)",
    )
    p.add_argument(
        "--sigma-z",
        type=float,
        default=0.50,
        help="Measurement noise std on observed quantity (default: 0.50)",
    )
    p.add_argument(
        "--nsims",
        type=int,
        default=20000,
        help="Number of Monte-Carlo forward simulations (default: 20000)",
    )
    p.add_argument(
        "--dt-forward",
        type=float,
        default=1.0,
        help="Forward simulation step in days (default: 1.0)",
    )
    p.add_argument(
        "--max-days",
        type=float,
        default=3650.0,
        help="Maximum forward horizon in days (default: 3650)",
    )
    p.add_argument("--seed", type=int, default=None, help="Random seed")
    p.add_argument(
        "--allow-negative-rate",
        action="store_true",
        help="If set, simulated rate may go negative (i.e., net gains). Default clips at 0.",
    )
    p.add_argument(
        "--min-rate",
        type=float,
        default=0.0,
        help="Lower bound for rate in simulations (default: 0.0)",
    )
    p.add_argument(
        "--bins",
        type=int,
        default=24,
        help="Histogram bins for text output (default: 24)",
    )
    p.add_argument(
        "--quantiles",
        type=str,
        default="0.10,0.25,0.50,0.75,0.90",
        help="Comma-separated quantiles to report (default: 0.10,0.25,0.50,0.75,0.90)",
    )
    p.add_argument(
        "--keep-same-day",
        dest="drop_same_day",
        action="store_false",
        help="Keep multiple readings on the same day (default drops all but last)",
    )
    args = p.parse_args()

    try:
        qs = tuple(float(x) for x in args.quantiles.split(","))
    except Exception:
        sys.exit("Invalid --quantiles; use comma-separated floats in (0,1).")
    if not all(0 < q < 1 for q in qs):
        sys.exit("Quantiles must be in (0,1).")

    return Options(
        path=args.file,
        sigma_r=args.sigma_r,
        sigma_q=args.sigma_q,
        sigma_z=args.sigma_z,
        nsims=args.nsims,
        dt_forward=args.dt_forward,
        max_days=args.max_days,
        seed=args.seed,
        allow_negative_rate=args.allow_negative_rate,
        min_rate=args.min_rate,
        bins=args.bins,
        quantiles=qs,
        drop_same_day_duplicates=args.drop_same_day,
    )


def read_data(
    path: str, drop_same_day_duplicates: bool
) -> List[Tuple[date, float]]:
    """
    Read and parse the input file, returning (date, quantity) pairs sorted by date.

    Lines starting with '#' and blank lines are ignored. Both date and quantity
    parse errors cause the line to be silently skipped. When
    drop_same_day_duplicates is True, only the last reading per calendar day
    is kept.
    """
    rows: List[Tuple[date, float]] = []
    with open(path, "r", encoding="utf-8") as fh:
        for line in fh:
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            parts = s.split()
            if len(parts) < 2:
                continue
            try:
                d = datetime.strptime(parts[0], DATE_FMT).date()
                q = float(round(float(parts[1])))  # normalise to integer-like float
            except ValueError:
                continue
            rows.append((d, q))
    if not rows:
        sys.exit("No valid rows found.")
    rows.sort(key=lambda x: x[0])

    if drop_same_day_duplicates:
        # keep the last reading per day
        dedup: dict = {}
        for d, q in rows:
            dedup[d] = q
        rows = sorted(dedup.items(), key=lambda x: x[0])

    return rows


def compute_time_axis(
    rows: List[Tuple[date, float]],
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Convert (date, quantity) rows to float arrays with time in days from the
    first observation.

    Returns (t_days, quantities) where t_days[0] == 0.
    """
    d0 = rows[0][0]
    t_days = np.array([(d - d0).days for (d, _) in rows], dtype=float)
    quantities = np.array([qty for (_, qty) in rows], dtype=float)
    return t_days, quantities


def initial_rate_guess(t: np.ndarray, q: np.ndarray) -> float:
    """
    Estimate a starting consumption rate as the median of positive per-day
    decreases between consecutive observations.

    Only intervals with net consumption (dq > 0) are included; zero-change or
    restocking intervals are ignored. Falls back to 1e-6 if no positive rates
    are found, to avoid downstream divide-by-zero.
    """
    rates = []
    for i in range(1, len(t)):
        dt = t[i] - t[i - 1]
        if dt <= 0:
            continue
        dq = q[i - 1] - q[i]  # positive when quantity decreases
        r = dq / dt
        if r > 0:
            rates.append(r)
    if rates:
        return float(np.median(rates))
    return 1e-6


def kalman_filter_random_walk_rate(
    t: np.ndarray,
    z: np.ndarray,
    sigma_r: float,
    sigma_q: float,
    sigma_z: float,
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Run a linear Kalman filter over the observations and return the posterior
    mean and covariance at the last time step.

    State vector: x = [q, r]^T  (quantity, consumption rate)

    Dynamics (continuous-time diffusion, discretised over interval dt):
        q_{k+1} = q_k - r_k * dt + w_q,   w_q ~ N(0, sigma_q^2 * dt)
        r_{k+1} = r_k + w_r,               w_r ~ N(0, sigma_r^2 * dt)

    Observation:
        z_k = q_k + v,   v ~ N(0, sigma_z^2)

    Both process-noise variances scale with dt so that sigma_q and sigma_r
    are expressed as diffusion coefficients per sqrt(day), consistent with the
    forward simulation.  The covariance is updated using the Joseph form for
    numerical stability.  With fewer than two observations the rate cannot be
    estimated, so a vague prior centred on the initial rate guess is returned.

    Returns (x_T, P_T): posterior mean vector and 2×2 covariance matrix.
    """
    n = len(t)
    if n < 2:
        x = np.array([z[-1], initial_rate_guess(t, z)], dtype=float)
        P = np.diag([10.0**2, 1.0**2])
        return x, P

    r0 = initial_rate_guess(t, z)
    x = np.array([z[0], r0], dtype=float)
    P = np.diag([10.0**2, 1.0**2])

    H = np.array([[1.0, 0.0]])   # observe quantity only
    R_scalar = sigma_z**2        # measurement variance (scalar)
    I = np.eye(2)

    for k in range(1, n):
        dt = max(t[k] - t[k - 1], 1e-9)
        F = np.array([[1.0, -dt], [0.0, 1.0]])
        # Both noise terms scale with dt (diffusion model)
        Q = np.diag([sigma_q**2 * dt, sigma_r**2 * dt])

        # Predict
        x = F @ x
        P = F @ P @ F.T + Q

        # Innovation and Kalman gain; S is 1×1 so invert as a scalar
        S_scalar = float((H @ P @ H.T)[0, 0]) + R_scalar
        K = (P @ H.T) / S_scalar          # shape (2, 1)
        y = float(z[k]) - float((H @ x)[0])
        x = x + K.ravel() * y

        # Joseph form: P = (I-KH) P (I-KH)^T + K R K^T
        KH = K @ H
        P = (I - KH) @ P @ (I - KH).T + K @ np.array([[R_scalar]]) @ K.T

    # Posterior rate is a consumption model; enforce non-negative mean
    x[1] = max(x[1], 0.0)
    return x, P


def simulate_hitting_time(
    x_mean: np.ndarray,
    P: np.ndarray,
    nsims: int,
    sigma_r: float,
    sigma_q: float,
    dt_forward: float,
    max_days: float,
    rng: np.random.Generator,
    allow_negative_rate: bool,
    min_rate: float,
) -> np.ndarray:
    """
    Monte-Carlo forward simulation of hitting times (days until quantity <= 0).

    Each simulation draws an initial state from N(x_mean, P) and evolves under
    independent random-walk dynamics.  Both sigma_r and sigma_q are scaled by
    sqrt(dt_forward) so that the implied diffusion is consistent across
    different step sizes.  All nsims paths are advanced in parallel using numpy
    array operations; paths that have already depleted are excluded from
    subsequent steps.

    Returns an array of length nsims.  Paths that do not deplete within
    max_days are recorded as np.inf.
    """
    # Decompose P for correlated initial-state sampling
    try:
        L = np.linalg.cholesky(P)
    except np.linalg.LinAlgError:
        vals, vecs = np.linalg.eigh(P)
        vals = np.maximum(vals, 0.0)
        L = vecs @ np.diag(np.sqrt(vals))

    # Draw all initial (q, r) pairs at once: xi shape (2, nsims)
    xi = x_mean[:, None] + L @ rng.standard_normal((2, nsims))
    q = xi[0].copy()
    r = xi[1].copy()
    if not allow_negative_rate:
        np.clip(r, min_rate, None, out=r)

    hits = np.full(nsims, np.inf, dtype=float)
    active = np.ones(nsims, dtype=bool)   # True = not yet depleted

    sqrt_dt = math.sqrt(dt_forward)
    n_steps = math.ceil(max_days / dt_forward)

    for step in range(1, n_steps + 1):
        n_active = int(active.sum())
        if n_active == 0:
            break

        # Evolve rate: random walk scaled by sqrt(dt)
        r[active] += rng.normal(0.0, sigma_r * sqrt_dt, size=n_active)
        if not allow_negative_rate:
            r[active] = np.maximum(r[active], min_rate)

        # Evolve quantity: process noise + deterministic consumption
        q[active] += rng.normal(0.0, sigma_q * sqrt_dt, size=n_active)
        q[active] -= r[active] * dt_forward

        # Record paths that just crossed zero
        newly_hit = active & (q <= 0.0)
        hits[newly_hit] = step * dt_forward
        active &= ~newly_hit

    return hits


def ascii_histogram(data: np.ndarray, bins: int, width: int = 60) -> str:
    """
    Render a fixed-width ASCII bar chart of data values.

    Only finite values are plotted; each bar is scaled relative to the tallest
    bin.  Returns a plain message string if no finite values exist.
    """
    finite = data[np.isfinite(data)]
    if len(finite) == 0:
        return "(no depletion within horizon for any simulation)"
    counts, edges = np.histogram(finite, bins=bins)
    peak = counts.max()
    lines = []
    for i in range(len(counts)):
        bar_len = int(round(width * counts[i] / peak)) if peak > 0 else 0
        bar = "█" * bar_len
        lines.append(f"{edges[i]:8.1f}–{edges[i + 1]:8.1f} d | {bar} {counts[i]}")
    return "\n".join(lines)


def main():
    """Entry point: parse arguments, run Kalman filter + Monte Carlo, print results."""
    opt = parse_args()
    rng = np.random.default_rng(opt.seed)

    rows = read_data(opt.path, drop_same_day_duplicates=opt.drop_same_day_duplicates)
    t_days, q_obs = compute_time_axis(rows)

    xT, PT = kalman_filter_random_walk_rate(
        t_days,
        q_obs,
        sigma_r=opt.sigma_r,
        sigma_q=opt.sigma_q,
        sigma_z=opt.sigma_z,
    )

    q_now, r_now = float(xT[0]), float(xT[1])

    hits = simulate_hitting_time(
        x_mean=xT,
        P=PT,
        nsims=opt.nsims,
        sigma_r=opt.sigma_r,
        sigma_q=opt.sigma_q,
        dt_forward=opt.dt_forward,
        max_days=opt.max_days,
        rng=rng,
        allow_negative_rate=opt.allow_negative_rate,
        min_rate=opt.min_rate,
    )

    finite = hits[np.isfinite(hits)]
    censored = np.sum(~np.isfinite(hits))
    censored_pct = 100.0 * censored / len(hits)

    print("=== Time-to-Empty Forecast (State-space RW rate + Monte Carlo) ===")
    print(
        f"Readings: {len(rows)}  |  Current q_now ≈ {q_now:.2f}  |  Current rate r_now ≈ {r_now:.4f} per day"
    )
    print(
        f"Model params: sigma_r={opt.sigma_r:.3f}/√day, sigma_q={opt.sigma_q:.3f}/√day, sigma_z={opt.sigma_z:.3f}"
    )
    print(
        f"Simulations: {opt.nsims}  |  step={opt.dt_forward} day  |  horizon={opt.max_days} days"
    )
    if censored > 0:
        print(
            f"Note: {censored} simulations ({censored_pct:.1f}%) did NOT reach zero within horizon."
        )

    if len(finite) == 0:
        print(
            "No depletion expected within the chosen horizon given current model/settings."
        )
        sys.exit(0)

    qs = np.quantile(finite, q=opt.quantiles)
    qlabels = ", ".join(
        f"{int(100*q):>2d}%={v:,.1f} d" for q, v in zip(opt.quantiles, qs)
    )
    print("Quantiles:", qlabels)

    thresholds = sorted(set(float(round(v / 10) * 10) for v in qs))
    if thresholds:
        probs = []
        for th in thresholds:
            p = 100.0 * (finite >= th).mean()
            probs.append(f"P[T ≥ {th:.0f} d] = {p:5.1f}%")
        print("Threshold survival:", " | ".join(probs))

    print("\nHistogram of time-to-empty (days):")
    print(ascii_histogram(finite, bins=opt.bins))

    med = float(np.median(finite))
    lo = float(np.quantile(finite, 0.25))
    hi = float(np.quantile(finite, 0.75))
    print(
        f"\nSummary: median ≈ {med:.1f} days (IQR {lo:.1f}–{hi:.1f}), "
        f"{'with long tail' if censored_pct > 0 else 'finite for nearly all sims'}."
    )


if __name__ == "__main__":
    main()
