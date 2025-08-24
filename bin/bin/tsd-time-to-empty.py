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
    python time_to_empty.py -f data.txt
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
        help="Process noise std on quantity per step (default: 0.25)",
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
        sigma_r=float(args.sigma_r),
        sigma_q=float(args.sigma_q),
        sigma_z=float(args.sigma_z),
        nsims=int(args.nsims),
        dt_forward=float(args.dt_forward),
        max_days=float(args.max_days),
        seed=args.seed,
        allow_negative_rate=bool(args.allow_negative_rate),
        min_rate=float(args.min_rate),
        bins=int(args.bins),
        quantiles=qs,
        drop_same_day_duplicates=bool(args.drop_same_day),
    )


def read_data(
    path: str, drop_same_day_duplicates: bool
) -> List[Tuple[date, float]]:
    rows: List[Tuple[date, float]] = []
    with open(path, "r", encoding="utf-8") as fh:
        for line in fh:
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            parts = s.split()
            if len(parts) < 2:
                continue
            d = datetime.strptime(parts[0], DATE_FMT).date()
            try:
                q = float(parts[1])
            except ValueError:
                continue
            rows.append((d, float(int(round(q)))))  # ensure integer-like
    if not rows:
        sys.exit("No valid rows found.")
    # sort by date
    rows.sort(key=lambda x: x[0])

    if drop_same_day_duplicates:
        # keep the last reading per day
        dedup = {}
        for d, q in rows:
            dedup[d] = q
        rows = sorted(dedup.items(), key=lambda x: x[0])

    return rows


def compute_time_axis(
    rows: List[Tuple[date, float]],
) -> Tuple[np.ndarray, np.ndarray]:
    """Return (t_days, q) arrays, t_days starting at 0."""
    d0 = rows[0][0]
    t_days = np.array([(d - d0).days for (d, _) in rows], dtype=float)
    q = np.array([q for (_, q) in rows], dtype=float)
    return t_days, q


def initial_rate_guess(t: np.ndarray, q: np.ndarray) -> float:
    """Median of positive per-day decreases over strictly increasing time."""
    rates = []
    for i in range(1, len(t)):
        dt = t[i] - t[i - 1]
        if dt <= 0:
            continue
        dq = q[i - 1] - q[i]  # consumption (positive if decreasing)
        r = dq / dt
        if r > 0:
            rates.append(r)
    if rates:
        return float(np.median(rates))
    # Fallback: tiny rate to avoid divide-by-zero downstream
    return 1e-6


def kalman_filter_random_walk_rate(
    t: np.ndarray,
    z: np.ndarray,
    sigma_r: float,
    sigma_q: float,
    sigma_z: float,
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Linear Gaussian state: x = [q, r]^T
      Predict: q_{k+1} = q_k - r_k * dt + w_q;   r_{k+1} = r_k + w_r,  w_r ~ N(0, (sigma_r^2)*dt)
      Observe: z_k = q_k + v, v ~ N(0, sigma_z^2)

    Returns: (x_T, P_T) for the final time T.
    """
    n = len(t)
    if n < 2:
        # with 1 reading we cannot learn rate; just return q with vague rate
        x = np.array([z[-1], initial_rate_guess(t, z)], dtype=float)
        P = np.diag([10.0**2, 1.0**2])
        return x, P

    # Initialize
    r0 = initial_rate_guess(t, z)
    x = np.array([z[0], r0], dtype=float)
    P = np.diag([10.0**2, 1.0**2])  # fairly vague

    H = np.array([[1.0, 0.0]])  # we observe q only
    R = np.array([[sigma_z**2]])  # measurement noise

    I = np.eye(2)

    for k in range(1, n):
        dt = max(t[k] - t[k - 1], 1e-9)  # robust
        # State transition
        F = np.array([[1.0, -dt], [0.0, 1.0]])
        # Process noise; we allow a small sigma_q plus rate RW scaling with dt
        Q = np.diag([sigma_q**2, (sigma_r**2) * dt])

        # Predict
        x = F @ x
        P = F @ P @ F.T + Q

        # Update with measurement z[k]
        zk = np.array([[z[k]]])
        y = zk - (H @ x)  # innovation
        S = H @ P @ H.T + R
        K = P @ H.T @ np.linalg.inv(S)  # Kalman gain
        x = x + (K @ y).reshape(-1)
        # Joseph form for numerical stability
        KH = K @ H
        P = (I - KH) @ P @ (I - KH).T + K @ R @ K.T

    # Enforce non-negative rate mean in the filtered state (model assumes consumption >= 0)
    if x[1] < 0:
        x[1] = 0.0
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
    """Return array of hitting times (days). np.inf if not emptied within max_days."""
    # Cholesky (fallback to eigen if P not PSD)
    try:
        L = np.linalg.cholesky(P)
    except np.linalg.LinAlgError:
        vals, vecs = np.linalg.eigh(P)
        vals[vals < 0] = 0
        L = vecs @ np.diag(np.sqrt(vals))

    hits = np.full(nsims, np.inf, dtype=float)

    for s in range(nsims):
        # Sample initial state
        xi = x_mean + L @ rng.standard_normal(2)
        q, r = float(xi[0]), float(xi[1])
        if not allow_negative_rate:
            r = max(r, min_rate)

        t_accum = 0.0
        while t_accum < max_days:
            # Random-walk rate
            r += rng.normal(0.0, sigma_r * math.sqrt(dt_forward))
            if not allow_negative_rate:
                r = max(r, min_rate)
            # Evolve quantity
            q += rng.normal(0.0, sigma_q)  # small model wiggle
            q -= r * dt_forward
            t_accum += dt_forward
            if q <= 0.0:
                hits[s] = t_accum
                break

    return hits


def ascii_histogram(data: np.ndarray, bins: int, width: int = 60) -> str:
    finite = data[np.isfinite(data)]
    if len(finite) == 0:
        return "(no depletion within horizon for any simulation)"
    counts, edges = np.histogram(finite, bins=bins)
    peak = counts.max() if counts.size else 1
    lines = []
    for i in range(len(counts)):
        left = edges[i]
        right = edges[i + 1]
        bar_len = int(round(width * counts[i] / peak)) if peak > 0 else 0
        bar = "█" * bar_len
        lines.append(f"{left:8.1f}–{right:8.1f} d | {bar} {counts[i]}")
    return "\n".join(lines)


def main():
    opt = parse_args()
    if opt.seed is not None:
        rng = np.random.default_rng(opt.seed)
    else:
        rng = np.random.default_rng()

    rows = read_data(
        opt.path, drop_same_day_duplicates=opt.drop_same_day_duplicates
    )
    t_days, q_obs = compute_time_axis(rows)

    # Kalman filter to estimate current (q, r) with uncertainty
    xT, PT = kalman_filter_random_walk_rate(
        t_days,
        q_obs,
        sigma_r=opt.sigma_r,
        sigma_q=opt.sigma_q,
        sigma_z=opt.sigma_z,
    )

    q_now, r_now = float(xT[0]), float(xT[1])

    # Monte-Carlo forward hitting time distribution
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

    # Report
    print("=== Time-to-Empty Forecast (State-space RW rate + Monte Carlo) ===")
    print(
        f"Readings: {len(rows)}  |  Current q_now ≈ {q_now:.2f}  |  Current rate r_now ≈ {r_now:.4f} per day"
    )
    print(
        f"Model params: sigma_r={opt.sigma_r:.3f}/√day, sigma_q={opt.sigma_q:.3f}, sigma_z={opt.sigma_z:.3f}"
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

    # Simple probabilities over some thresholds (auto-picked from quantiles)
    thresholds = sorted(
        set(float(round(v / 10) * 10) for v in qs)
    )  # rounded days
    if thresholds:
        probs = []
        for th in thresholds:
            p = 100.0 * (finite >= th).mean()
            probs.append(f"P[T ≥ {th:.0f} d] = {p:5.1f}%")
        print("Threshold survival:", " | ".join(probs))

    print("\nHistogram of time-to-empty (days):")
    print(ascii_histogram(finite, bins=opt.bins))

    # Friendly concluding line
    med = float(np.median(finite))
    lo = float(np.quantile(finite, 0.25))
    hi = float(np.quantile(finite, 0.75))
    print(
        f"\nSummary: median ≈ {med:.1f} days (IQR {lo:.1f}–{hi:.1f}), "
        f"{'with long tail' if censored_pct>0 else 'finite for nearly all sims'}."
    )


if __name__ == "__main__":
    main()
