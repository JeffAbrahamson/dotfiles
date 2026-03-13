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

Single-file usage:
    tsd-time-to-empty.py -f data.txt

Multi-file usage (files resolved relative to $TSD_DIR):
    tsd-time-to-empty.py widget sprocket
"""

import argparse
import math
import os
import subprocess
import sys
from dataclasses import dataclass
from datetime import date, datetime
from typing import List, Optional, Tuple

import numpy as np

DATE_FMT = "%Y-%m-%d"


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------


@dataclass
class Options:
    files: List[Tuple[str, str]]  # (label, path) pairs
    not_found: List[str]          # patterns that had no match in tsd -L
    multi_file_mode: bool
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
    fractional: bool
    auto_size: bool
    hist_min: float


@dataclass
class FileResult:
    """Processed results for a single input file."""

    label: str
    n_rows: int
    q_now: float
    r_now: float
    hits: np.ndarray
    finite: np.ndarray
    censored: int
    censored_pct: float
    already_empty: bool = False  # True when q_now <= 0 at forecast time
    error: Optional[str] = None  # set when the file could not be processed


# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------


def _resolve_name_via_tsd_list(
    pattern: str, tsd_dir: str
) -> List[Tuple[str, str]]:
    """Run ``tsd -L`` and return (name, path) pairs whose name contains pattern."""
    try:
        result = subprocess.run(
            ["tsd.py", "-L"], capture_output=True, text=True, check=True
        )
    except subprocess.CalledProcessError as e:
        sys.exit(f"`tsd -L` failed: {e.stderr.strip() or e}")
    matches = []
    for line in result.stdout.splitlines():
        name = line.strip()
        if name and pattern in name:
            matches.append((name, os.path.join(tsd_dir, name)))
    return matches


def parse_args() -> Options:
    """
    Parse command-line arguments and return a validated Options object.

    Files may be supplied either as positional names resolved under $TSD_DIR
    (enabling multi-file mode with a summary table) or via -f/--file for
    single-file mode that preserves the classic verbose output.
    """
    p = argparse.ArgumentParser(
        description="State-space random-walk rate + Monte-Carlo time-to-empty estimator",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    # --- input ---
    inp = p.add_argument_group("input")
    inp.add_argument(
        "files",
        nargs="*",
        metavar="FILE",
        help="File names relative to $TSD_DIR (enables multi-file mode)",
    )
    inp.add_argument(
        "-f",
        "--file",
        dest="file_path",
        metavar="PATH",
        help="Path to a single data file",
    )
    inp.add_argument(
        "--keep-same-day",
        dest="drop_same_day",
        action="store_false",
        help="Keep all readings on the same day (default: keep only the last)",
    )

    # --- Kalman filter ---
    kf = p.add_argument_group("Kalman filter")
    kf.add_argument(
        "--sigma-r",
        type=float,
        default=0.50,
        help="Rate random-walk diffusion per √day (default: 0.50)",
    )
    kf.add_argument(
        "--sigma-q",
        type=float,
        default=0.25,
        help="Quantity process-noise diffusion per √day (default: 0.25)",
    )
    kf.add_argument(
        "--sigma-z",
        type=float,
        default=0.50,
        help="Measurement noise std on observed quantity (default: 0.50)",
    )

    # --- Monte Carlo ---
    mc = p.add_argument_group("Monte Carlo")
    mc.add_argument(
        "--nsims",
        type=int,
        default=20000,
        help="Number of forward simulations (default: 20000)",
    )
    mc.add_argument(
        "--dt-forward",
        type=float,
        default=1.0,
        help="Simulation step in days (default: 1.0)",
    )
    mc.add_argument(
        "--max-days",
        type=float,
        default=3650.0,
        help="Forward horizon in days (default: 3650)",
    )
    mc.add_argument(
        "--seed",
        type=int,
        default=None,
        help="Random seed for reproducibility",
    )
    mc.add_argument(
        "--allow-negative-rate",
        action="store_true",
        help="Allow simulated rate to go negative (net gains); default clips to 0",
    )
    mc.add_argument(
        "--min-rate",
        type=float,
        default=0.0,
        help="Lower bound for simulated rate (default: 0.0)",
    )

    # --- display ---
    disp = p.add_argument_group("display")
    disp.add_argument(
        "--bins",
        type=int,
        default=24,
        help="Target number of histogram bins (default: 24)",
    )
    disp.add_argument(
        "--quantiles",
        type=str,
        default="0.10,0.25,0.50,0.75,0.90",
        help="Comma-separated quantiles to report (default: 0.10,0.25,0.50,0.75,0.90)",
    )
    disp.add_argument(
        "--fractional",
        action="store_true",
        help="Display fractional days; default rounds to whole days",
    )
    disp.add_argument(
        "--hist-min",
        type=float,
        default=0.0,
        help="Earliest day shown in histogram (default: 0); ignored with --auto-size",
    )
    disp.add_argument(
        "--auto-size",
        action="store_true",
        help="Fit histogram x-axis to where the data has mass; overrides --hist-min",
    )

    args = p.parse_args()

    # Validate quantiles
    try:
        qs = tuple(float(x) for x in args.quantiles.split(","))
    except Exception:
        sys.exit("Invalid --quantiles; use comma-separated floats in (0,1).")
    if not all(0 < q < 1 for q in qs):
        sys.exit("Quantiles must be in (0,1).")

    # Resolve file list
    multi_file_mode = bool(args.files)
    files: List[Tuple[str, str]] = []
    not_found: List[str] = []

    if args.files:
        tsd_dir = os.environ.get("TSD_DIR", "")
        if not tsd_dir:
            sys.exit(
                "$TSD_DIR is not set; required for positional file arguments."
            )
        for name in args.files:
            path = os.path.join(tsd_dir, name)
            if os.path.exists(path):
                files.append((name, path))
            else:
                matches = _resolve_name_via_tsd_list(name, tsd_dir)
                if matches:
                    files.extend(matches)
                else:
                    not_found.append(name)
        # Deduplicate by path while preserving order
        seen: set = set()
        unique: List[Tuple[str, str]] = []
        for label, path in files:
            if path not in seen:
                seen.add(path)
                unique.append((label, path))
        files = unique

    if args.file_path:
        label = os.path.basename(args.file_path)
        files.append((label, args.file_path))
        if not args.files:
            multi_file_mode = False  # -f alone → single-file mode

    if not files:
        p.error("Specify at least one file: use FILE arguments or -f/--file.")

    return Options(
        files=files,
        not_found=not_found,
        multi_file_mode=multi_file_mode,
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
        fractional=args.fractional,
        auto_size=args.auto_size,
        hist_min=args.hist_min,
    )


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------


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
                q = float(
                    round(float(parts[1]))
                )  # normalise to integer-like float
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


# ---------------------------------------------------------------------------
# Model
# ---------------------------------------------------------------------------


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

    H = np.array([[1.0, 0.0]])  # observe quantity only
    R_scalar = sigma_z**2  # measurement variance (scalar)
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
        K = (P @ H.T) / S_scalar  # shape (2, 1)
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
    active = np.ones(nsims, dtype=bool)  # True = not yet depleted

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


# ---------------------------------------------------------------------------
# Per-file computation
# ---------------------------------------------------------------------------


def _make_error_result(label: str, msg: str) -> FileResult:
    empty = np.array([], dtype=float)
    return FileResult(
        label=label, n_rows=0, q_now=0.0, r_now=0.0,
        hits=empty, finite=empty, censored=0, censored_pct=0.0,
        error=msg,
    )


def process_file(
    label: str,
    path: str,
    opt: Options,
    rng: np.random.Generator,
) -> FileResult:
    """Run the Kalman filter and (if not already empty) Monte Carlo simulation.

    When the Kalman-filtered quantity estimate is at or below zero the item is
    already depleted; the simulation is skipped and already_empty is set on the
    returned result.
    """
    try:
        rows = read_data(path, drop_same_day_duplicates=opt.drop_same_day_duplicates)
    except OSError as e:
        return _make_error_result(label, str(e))

    t_days, q_obs = compute_time_axis(rows)
    xT, PT = kalman_filter_random_walk_rate(
        t_days,
        q_obs,
        sigma_r=opt.sigma_r,
        sigma_q=opt.sigma_q,
        sigma_z=opt.sigma_z,
    )
    q_now = float(xT[0])
    r_now = float(xT[1])

    if q_obs[-1] <= 0.0:
        empty_arr = np.array([], dtype=float)
        return FileResult(
            label=label,
            n_rows=len(rows),
            q_now=q_now,
            r_now=r_now,
            hits=empty_arr,
            finite=empty_arr,
            censored=0,
            censored_pct=0.0,
            already_empty=True,
        )

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
    censored = int(np.sum(~np.isfinite(hits)))
    return FileResult(
        label=label,
        n_rows=len(rows),
        q_now=q_now,
        r_now=r_now,
        hits=hits,
        finite=finite,
        censored=censored,
        censored_pct=100.0 * censored / len(hits),
    )


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------


def fmt_days(v: float, fractional: bool) -> str:
    """Format a day count as a fractional (1 d.p.) or rounded whole number."""
    if fractional:
        return f"{v:,.1f}"
    return f"{int(round(v)):,d}"


def ascii_histogram(
    data: np.ndarray,
    bins: int,
    width: int = 60,
    hist_min: float = 0.0,
    auto_size: bool = False,
    fractional: bool = True,
) -> str:
    """
    Render a fixed-width ASCII bar chart of data values.

    Only finite values are plotted; each bar is scaled relative to the tallest
    bin.  When auto_size is False the x-axis starts at hist_min (default 0),
    showing empty leading bars when the data arrives later — useful for seeing
    how quickly depletion becomes probable.  When auto_size is True the axis
    is fitted to where the data has mass.  When fractional is False, bin
    boundaries are snapped to whole days.  Returns a plain message string if
    no finite values exist.
    """
    finite = data[np.isfinite(data)]
    if len(finite) == 0:
        return "(no depletion within horizon for any simulation)"

    data_max = float(np.max(finite))

    if fractional:
        if auto_size:
            hist_range = None
        else:
            lo = min(float(hist_min), data_max)
            hist_range = (lo, data_max)
        counts, edges = np.histogram(finite, bins=bins, range=hist_range)
        fmt = lambda v: f"{v:.1f}"
    else:
        if auto_size:
            lo = int(math.floor(float(np.min(finite))))
        else:
            lo = min(int(math.floor(hist_min)), int(math.floor(data_max)))
        hi = int(math.ceil(data_max))
        bin_width = max(1, math.ceil((hi - lo) / bins))
        bin_edges = np.arange(lo, hi + bin_width, bin_width, dtype=float)
        counts, edges = np.histogram(finite, bins=bin_edges)
        fmt = lambda v: f"{int(round(v))}"

    peak = counts.max()
    lines = []
    for i in range(len(counts)):
        bar_len = int(round(width * counts[i] / peak)) if peak > 0 else 0
        bar = "█" * bar_len
        lines.append(
            f"{fmt(edges[i]):>8}–{fmt(edges[i + 1]):>8} d | {bar} {counts[i]}"
        )
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Output: single-file mode
# ---------------------------------------------------------------------------


def _print_single(result: FileResult, opt: Options) -> None:
    """Print the full verbose report for a single file."""
    if result.error:
        sys.exit(f"ERROR: {result.error}")
    print("=== Time-to-Empty Forecast (State-space RW rate + Monte Carlo) ===")
    print(
        f"Readings: {result.n_rows}"
        f"  |  Current q_now ≈ {result.q_now:.2f}"
        f"  |  Current rate r_now ≈ {result.r_now:.4f} per day"
    )

    if result.already_empty:
        print(
            "Already empty (last observed value is 0); no forecast produced."
        )
        sys.exit(0)

    print(
        f"Model params: sigma_r={opt.sigma_r:.3f}/√day,"
        f" sigma_q={opt.sigma_q:.3f}/√day, sigma_z={opt.sigma_z:.3f}"
    )
    print(
        f"Simulations: {opt.nsims}"
        f"  |  step={opt.dt_forward} day"
        f"  |  horizon={opt.max_days} days"
    )
    if result.censored > 0:
        print(
            f"Note: {result.censored} simulations"
            f" ({result.censored_pct:.1f}%) did NOT reach zero within horizon."
        )

    if len(result.finite) == 0:
        print(
            "No depletion expected within the chosen horizon given current model/settings."
        )
        sys.exit(0)

    qs = np.quantile(result.finite, q=opt.quantiles)
    qlabels = ", ".join(
        f"{int(100*q):>2d}%={fmt_days(float(v), opt.fractional)} d"
        for q, v in zip(opt.quantiles, qs)
    )
    print("Quantiles:", qlabels)

    thresholds = sorted(set(float(round(v / 10) * 10) for v in qs))
    if thresholds:
        probs = [
            f"P[T ≥ {fmt_days(th, opt.fractional)} d] = {100.0*(result.finite >= th).mean():5.1f}%"
            for th in thresholds
        ]
        print("Threshold survival:", " | ".join(probs))

    print("\nHistogram of time-to-empty (days):")
    print(
        ascii_histogram(
            result.finite,
            bins=opt.bins,
            hist_min=opt.hist_min,
            auto_size=opt.auto_size,
            fractional=opt.fractional,
        )
    )

    med = fmt_days(float(np.median(result.finite)), opt.fractional)
    lo = fmt_days(float(np.quantile(result.finite, 0.25)), opt.fractional)
    hi = fmt_days(float(np.quantile(result.finite, 0.75)), opt.fractional)
    print(
        f"\nSummary: median ≈ {med} days (IQR {lo}–{hi}),"
        f" {'with long tail' if result.censored_pct > 0 else 'finite for nearly all sims'}."
    )


# ---------------------------------------------------------------------------
# Output: multi-file mode
# ---------------------------------------------------------------------------


def _section_header(label: str, total_width: int = 60) -> str:
    """Return a divider line of the form '── label ────────'."""
    prefix = f"── {label} "
    return prefix + "─" * max(0, total_width - len(prefix))


def _print_file_section(result: FileResult, opt: Options) -> None:
    """Print the labeled parameters-and-histogram section for one file."""
    print(_section_header(result.label))
    if result.error:
        print(f"ERROR: {result.error}\n")
        return
    print(
        f"Readings: {result.n_rows}"
        f"  |  q_now ≈ {result.q_now:.2f}"
        f"  |  rate ≈ {result.r_now:.4f}/day"
    )
    if result.already_empty:
        print("Already empty (last observed value is 0).\n")
        return
    if result.censored > 0:
        print(
            f"Note: {result.censored} sims"
            f" ({result.censored_pct:.1f}%) did NOT reach zero within horizon."
        )
    if len(result.finite) == 0:
        print("No depletion within horizon.\n")
        return
    print("\nHistogram of time-to-empty (days):")
    print(
        ascii_histogram(
            result.finite,
            bins=opt.bins,
            hist_min=opt.hist_min,
            auto_size=opt.auto_size,
            fractional=opt.fractional,
        )
    )
    print()


def _median_sort_key(result: FileResult) -> float:
    """Return the median hitting time for sorting; inf if already empty, error, or no finite hits."""
    if result.error or result.already_empty or len(result.finite) == 0:
        return float("inf")
    return float(np.median(result.finite))


def _print_summary_table(results: List[FileResult], opt: Options) -> None:
    """
    Print the quantile summary table, sorted by median in descending order
    (most time remaining first).  Files with no finite hits appear at the end.
    """

    # Descending: negate finite medians; already-empty and no-hits go to the end; errors last
    def _sort_key(r: FileResult) -> Tuple[int, float]:
        if r.error:
            return (3, 0.0)
        if r.already_empty:
            return (2, 0.0)
        if len(r.finite) == 0:
            return (1, 0.0)
        return (0, -_median_sort_key(r))

    sorted_results = sorted(results, key=_sort_key)

    q_headers = [f"P{int(100*q)}" for q in opt.quantiles]
    name_w = max(4, max(len(r.label) for r in results))
    val_w = 8 if opt.fractional else 6

    print("\n=== Summary (sorted by median, descending) ===\n")
    header = f"{'Name':{name_w}s}"
    for h in q_headers:
        header += f"  {h:>{val_w}s}"
    header += f"  {'censored':>9s}"
    print(header)
    print("─" * len(header))

    for r in sorted_results:
        row = f"{r.label:{name_w}s}"
        if r.error:
            tag = "error"
            for _ in q_headers:
                row += f"  {tag:>{val_w}s}"
                tag = ""
        elif r.already_empty:
            tag = "empty"
            for _ in q_headers:
                row += f"  {tag:>{val_w}s}"
                tag = ""
        elif len(r.finite) == 0:
            for _ in q_headers:
                row += f"  {'—':>{val_w}s}"
        else:
            qs = np.quantile(r.finite, q=opt.quantiles)
            for v in qs:
                row += f"  {fmt_days(float(v), opt.fractional):>{val_w}s}"
        row += f"  {r.censored_pct:>8.1f}%"
        print(row)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main() -> None:
    """Parse arguments, run Kalman filter + Monte Carlo for each file, print results."""
    opt = parse_args()
    rng = np.random.default_rng(opt.seed)

    results: List[FileResult] = []
    for label, path in opt.files:
        results.append(process_file(label, path, opt, rng))
    for name in opt.not_found:
        results.append(_make_error_result(name, f"no series matching {name!r} found in tsd -L"))

    if opt.multi_file_mode:
        print(
            f"Model: sigma_r={opt.sigma_r:.3f}"
            f"  sigma_q={opt.sigma_q:.3f}"
            f"  sigma_z={opt.sigma_z:.3f}"
        )
        print(
            f"Simulations: {opt.nsims}"
            f"  |  step={opt.dt_forward} d"
            f"  |  horizon={opt.max_days} d"
        )
        print()
        for r in results:
            _print_file_section(r, opt)
        _print_summary_table(results, opt)
    else:
        _print_single(results[0], opt)

    if any(r.error for r in results):
        sys.exit(1)


if __name__ == "__main__":
    main()
