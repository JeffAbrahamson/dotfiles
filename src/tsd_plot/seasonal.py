"""Seasonal scatter plot CLI for TSD data files."""

from __future__ import annotations

import argparse
import calendar
import datetime as dt
import math
from dataclasses import dataclass
from typing import Dict, Iterable, List, Sequence, Tuple

import matplotlib.pyplot as plt
import seaborn as sns

from .cli import (
    PrefixMatchError,
    SeriesData,
    format_title,
    parse_filespec,
    read_series,
    resolve_prefix,
    resolve_tsd_dir,
    sum_series,
)

PERIODS = {"year", "month", "week"}
HEATMAP_MODES = {"sum", "mean", "count"}
HEATMAP_STYLES = {"seasonal", "evolving"}
WEEKDAY_LABELS = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
DEFAULT_MIN_SIZE = 5.0
DEFAULT_MAX_SIZE = 16.0
DEFAULT_HEATMAP_ALPHA = 0.35
DEFAULT_HEATMAP_SIGMA = {
    "year": 10.0,
    "month": 2.0,
    "week": 0.75,
}
DEFAULT_HEATMAP_SIGMA_Y = 0.75
HELP_OVERVIEW = """\
Plot repeated-season views of TSD time series to spot seasonal structure.

The x-axis shows position within a repeated period such as year, month, or
week. The y-axis stacks rows by year-like buckets, so repeated vertical bands
indicate stable seasonality while diagonal trends can reveal drift over time.

Option groups:
  input and grouping   choose files, period, and summation behaviour
  point appearance     control title, color, dot size, and dot opacity
  heatmap overlay      add a smoothed background to emphasise seasonal zones
  reference guides     control month-boundary guide lines in year view
  diagnostics          print resolved settings and data summaries
"""


@dataclass(frozen=True)
class PeriodPoint:
    """A plotted point located within a repeated time period."""

    row_key: str
    x_value: float


def is_leap_year(year: int) -> bool:
    """Return whether *year* is a leap year."""
    return calendar.isleap(year)


def period_point(date: dt.date, period: str) -> PeriodPoint:
    """Map a date to its seasonal row and x-axis position."""
    if period == "year":
        return PeriodPoint(
            row_key=str(date.year),
            x_value=float(date.timetuple().tm_yday - 1),
        )
    if period == "month":
        return PeriodPoint(
            row_key=f"{date.year:04d}-{date.month:02d}",
            x_value=float(date.day - 1),
        )
    if period == "week":
        iso_year, iso_week, iso_day = date.isocalendar()
        return PeriodPoint(
            row_key=f"{iso_year:04d}-W{iso_week:02d}",
            x_value=float(iso_day - 1),
        )
    raise ValueError(f"Unsupported period {period!r}")


def project_series_to_period(
    series: SeriesData, period: str
) -> List[Tuple[str, float, float]]:
    """Project a series into repeated-period plotting coordinates."""
    projected: List[Tuple[str, float, float]] = []
    for date, value in series.sorted_points():
        point = period_point(date, period)
        projected.append((point.row_key, point.x_value, value))
    return projected


def size_map(
    values: Sequence[float], min_diameter: float, max_diameter: float
) -> List[float]:
    """Map values to matplotlib scatter areas using diameter bounds."""
    if not values:
        return []
    if min_diameter <= 0 or max_diameter <= 0:
        raise ValueError("Point sizes must be positive")
    if min_diameter > max_diameter:
        raise ValueError("Minimum point size cannot exceed maximum")

    low = min(values)
    high = max(values)
    if high == low:
        diameter = (min_diameter + max_diameter) / 2.0
        return [diameter**2 for _ in values]

    areas: List[float] = []
    for value in values:
        ratio = (value - low) / (high - low)
        diameter = min_diameter + ratio * (max_diameter - min_diameter)
        areas.append(diameter**2)
    return areas


def resolve_size_bounds(
    min_size: float | None, max_size: float | None
) -> Tuple[float, float]:
    """Resolve point-size bounds while keeping one-sided overrides valid."""
    resolved_min = DEFAULT_MIN_SIZE if min_size is None else min_size
    resolved_max = DEFAULT_MAX_SIZE if max_size is None else max_size

    if (
        min_size is not None
        and max_size is None
        and resolved_min > resolved_max
    ):
        resolved_max = resolved_min
    if (
        max_size is not None
        and min_size is None
        and resolved_max < resolved_min
    ):
        resolved_min = resolved_max

    return resolved_min, resolved_max


def period_span(period: str, year_length: int) -> int:
    """Return the horizontal span for the chosen period."""
    if period == "year":
        return year_length
    if period == "month":
        return 31
    if period == "week":
        return 7
    raise ValueError(f"Unsupported period {period!r}")


def resolve_heatmap_sigma(period: str, sigma: float | None) -> float:
    """Return the smoothing width for the seasonal heatmap."""
    if sigma is not None:
        return sigma
    return DEFAULT_HEATMAP_SIGMA[period]


def gaussian_weight(distance: float, sigma: float) -> float:
    """Return a Gaussian kernel weight for *distance* and *sigma*."""
    return math.exp(-((distance**2) / (2.0 * sigma**2)))


def normalize_signal(values: Sequence[float]) -> List[float]:
    """Scale a signal to the range 0..1."""
    if not values:
        return []
    high = max(values)
    if high <= 0:
        return [0.0 for _ in values]
    return [value / high for value in values]


def heatmap_numerator_term(mode: str, weight: float, value: float) -> float:
    """Return the numerator contribution for one point."""
    if mode == "count":
        return weight
    if mode in {"sum", "mean"}:
        return weight * value
    raise ValueError(f"Unsupported heatmap mode {mode!r}")


def compute_heatmap_signal(  # noqa: CCR001
    projected: Sequence[Tuple[str, float, float]],
    *,
    span: int,
    mode: str,
    sigma: float,
) -> List[float]:
    """Compute a smoothed seasonal heatmap signal across the x-axis."""
    if not projected:
        return [0.0 for _ in range(span)]

    if sigma <= 0:
        raise ValueError("Heatmap sigma must be positive")

    numerator = [0.0 for _ in range(span)]
    denominator = [0.0 for _ in range(span)]

    for _, x_value, value in projected:
        for index in range(span):
            weight = gaussian_weight(index - x_value, sigma)
            denominator[index] += weight
            numerator[index] += heatmap_numerator_term(mode, weight, value)

    if mode == "mean":
        raw_values = [
            0.0 if count == 0 else total / count
            for total, count in zip(numerator, denominator)
        ]
    else:
        raw_values = numerator

    return normalize_signal(raw_values)


def normalize_matrix(matrix: Sequence[Sequence[float]]) -> List[List[float]]:
    """Scale a matrix to the range 0..1."""
    flattened = [value for row in matrix for value in row]
    if not flattened:
        return []
    high = max(flattened)
    if high <= 0:
        return [[0.0 for value in row] for row in matrix]
    return [[value / high for value in row] for row in matrix]


def build_mean_heatmap(
    numerator: Sequence[Sequence[float]],
    denominator: Sequence[Sequence[float]],
) -> List[List[float]]:
    """Return cell-wise averages from numerator and denominator grids."""
    raw_values = []
    for row_num, row_den in zip(numerator, denominator):
        raw_values.append(
            [
                0.0 if count == 0 else total / count
                for total, count in zip(row_num, row_den)
            ]
        )
    return raw_values


def compute_evolving_heatmap(  # noqa: CCR001
    projected: Sequence[Tuple[str, float, float]],
    *,
    row_positions: Dict[str, int],
    row_count: int,
    span: int,
    mode: str,
    sigma_x: float,
    sigma_y: float,
) -> List[List[float]]:
    """Compute a smoothed 2D heatmap that preserves change across rows."""
    if not projected:
        return [[0.0 for _ in range(span)] for _ in range(row_count)]
    if sigma_x <= 0 or sigma_y <= 0:
        raise ValueError("Heatmap sigmas must be positive")

    numerator = [[0.0 for _ in range(span)] for _ in range(row_count)]
    denominator = [[0.0 for _ in range(span)] for _ in range(row_count)]

    for row_key, x_value, value in projected:
        point_row = row_positions[row_key]
        for row_index in range(row_count):
            row_weight = gaussian_weight(row_index - point_row, sigma_y)
            for column_index in range(span):
                column_weight = gaussian_weight(
                    column_index - x_value, sigma_x
                )
                weight = row_weight * column_weight
                denominator[row_index][column_index] += weight
                numerator[row_index][column_index] += heatmap_numerator_term(
                    mode, weight, value
                )

    raw_values = (
        build_mean_heatmap(numerator, denominator)
        if mode == "mean"
        else numerator
    )

    return normalize_matrix(raw_values)


def render_heatmap(
    ax: plt.Axes,
    *,
    projected: Sequence[Tuple[str, float, float]],
    row_positions: Dict[str, int],
    row_count: int,
    span: int,
    style: str,
    mode: str,
    sigma_x: float,
    sigma_y: float,
    alpha: float,
) -> None:
    """Render either the collapsed or evolving seasonal heatmap."""
    if style == "seasonal":
        heat_values = compute_heatmap_signal(
            projected,
            span=span,
            mode=mode,
            sigma=sigma_x,
        )
        image = [heat_values, heat_values]
    elif style == "evolving":
        image = compute_evolving_heatmap(
            projected,
            row_positions=row_positions,
            row_count=row_count,
            span=span,
            mode=mode,
            sigma_x=sigma_x,
            sigma_y=sigma_y,
        )
    else:  # pragma: no cover - defensive branch
        raise ValueError(f"Unsupported heatmap style {style!r}")

    ax.imshow(
        image,
        cmap="YlOrRd",
        aspect="auto",
        interpolation="bilinear",
        alpha=alpha,
        extent=(-0.5, span - 0.5, -0.5, row_count - 0.5),
        origin="lower",
        zorder=0.1,
    )


def month_line_positions(use_leap_calendar: bool) -> List[int]:
    """Return day offsets for month-start guide lines."""
    reference_year = 2024 if use_leap_calendar else 2023
    positions: List[int] = []
    for month in range(2, 13):
        yday = dt.date(reference_year, month, 1).timetuple().tm_yday
        positions.append(yday - 1)
    return positions


def year_x_limit(dates: Iterable[dt.date]) -> int:
    """Return the x-axis extent for year plots."""
    years = sorted({date.year for date in dates})
    if not years:
        return 365
    if any(is_leap_year(year) for year in years):
        return 366
    return 365


def make_row_positions(
    projected: Sequence[Tuple[str, float, float]],
) -> Tuple[Dict[str, int], List[str]]:
    """Create top-down y positions for each plotted row."""
    row_labels = sorted({row_key for row_key, _, _ in projected}, reverse=True)
    return {label: idx for idx, label in enumerate(row_labels)}, row_labels


def configure_x_axis(
    ax: plt.Axes,
    period: str,
    x_limit: int,
    draw_month_lines: bool,
    use_leap_calendar: bool,
) -> None:
    """Configure x-axis ticks and optional guide lines."""
    if period == "year":
        month_starts = month_line_positions(use_leap_calendar)
        month_labels = [calendar.month_abbr[month] for month in range(1, 13)]
        ax.set_xlim(-1, x_limit)
        ax.set_xticks([0] + month_starts)
        ax.set_xticklabels(month_labels)
        ax.set_xlabel("Day of year")
        if draw_month_lines:
            for position in month_starts:
                ax.axvline(
                    position,
                    color="lightgray",
                    linewidth=0.8,
                    zorder=0,
                )
        return

    if period == "month":
        ax.set_xlim(-0.5, 30.5)
        ax.set_xticks([0, 6, 13, 20, 27, 30])
        ax.set_xticklabels(["1", "7", "14", "21", "28", "31"])
        ax.set_xlabel("Day of month")
        return

    ax.set_xlim(-0.5, 6.5)
    ax.set_xticks(range(7))
    ax.set_xticklabels(WEEKDAY_LABELS)
    ax.set_xlabel("Day of week")


def plot_seasonal_series(  # noqa: CCR001
    series_list: Sequence[SeriesData],
    *,
    period: str,
    title: str,
    color: str | None,
    min_size: float,
    max_size: float,
    alpha: float,
    heatmap: bool,
    heatmap_style: str,
    heatmap_mode: str,
    heatmap_sigma_x: float,
    heatmap_sigma_y: float,
    heatmap_alpha: float,
    show_month_lines: bool,
) -> plt.Figure:
    """Plot repeated-period scatter points for one or more series."""
    sns.set_theme(style="whitegrid")
    fig, ax = plt.subplots(figsize=(11, 7))
    ax.set_title(title)
    ax.set_ylabel("Year" if period == "year" else period.capitalize())

    if not series_list:
        configure_x_axis(
            ax,
            period,
            x_limit=365,
            draw_month_lines=False,
            use_leap_calendar=False,
        )
        fig.tight_layout()
        return fig

    projected_by_series = [
        (series.label, project_series_to_period(series, period))
        for series in series_list
    ]
    all_projected = [
        point for _, projected in projected_by_series for point in projected
    ]
    row_positions, row_labels = make_row_positions(all_projected)

    all_values = [value for _, _, value in all_projected]
    size_lookup = size_map(all_values, min_size, max_size)
    size_by_index = dict(enumerate(size_lookup))

    all_dates = [date for series in series_list for date, _ in series.points]
    use_leap_calendar = False
    if period == "year":
        years = sorted({date.year for date in all_dates})
        use_leap_calendar = bool(years) and all(
            is_leap_year(year) for year in years
        )
    x_span = period_span(
        period,
        year_length=year_x_limit(all_dates),
    )

    if heatmap and row_labels:
        render_heatmap(
            ax,
            projected=all_projected,
            row_positions=row_positions,
            row_count=len(row_labels),
            span=x_span,
            style=heatmap_style,
            mode=heatmap_mode,
            sigma_x=heatmap_sigma_x,
            sigma_y=heatmap_sigma_y,
            alpha=heatmap_alpha,
        )

    offset = 0
    for series_index, (label, projected) in enumerate(projected_by_series):
        x_values = [x_value for _, x_value, _ in projected]
        y_values = [row_positions[row_key] for row_key, _, _ in projected]
        sizes = [
            size_by_index[offset + point_index]
            for point_index in range(len(projected))
        ]
        offset += len(projected)
        default_colors = plt.rcParams["axes.prop_cycle"].by_key()["color"]
        scatter_color = (
            color
            if color is not None
            else default_colors[series_index % len(default_colors)]
        )
        ax.scatter(
            x_values,
            y_values,
            s=sizes,
            color=scatter_color,
            alpha=alpha,
            edgecolors="none",
            label=label,
        )

    ax.set_yticks(range(len(row_labels)))
    ax.set_yticklabels(row_labels)
    ax.invert_yaxis()

    configure_x_axis(
        ax,
        period,
        x_limit=x_span,
        draw_month_lines=show_month_lines and period == "year",
        use_leap_calendar=use_leap_calendar,
    )

    if len(series_list) > 1:
        ax.legend()

    fig.tight_layout()
    return fig


def create_parser() -> argparse.ArgumentParser:
    """Create the seasonal plot argument parser."""

    class HelpFormatter(
        argparse.ArgumentDefaultsHelpFormatter,
        argparse.RawDescriptionHelpFormatter,
    ):
        """Formatter combining defaults with wrapped overview text."""

    parser = argparse.ArgumentParser(
        description=HELP_OVERVIEW,
        formatter_class=HelpFormatter,
    )
    input_group = parser.add_argument_group("input and grouping")
    point_group = parser.add_argument_group("point appearance")
    heatmap_group = parser.add_argument_group("heatmap overlay")
    guide_group = parser.add_argument_group("reference guides")
    diagnostics_group = parser.add_argument_group("diagnostics")

    input_group.add_argument(
        "files",
        nargs="+",
        metavar="FILE[:LABEL]",
        help=(
            "Input file names located in $TSD, $TSD_DIR, or $HOME/tsd. "
            "Append :LABEL to customise the legend entry."
        ),
    )
    input_group.add_argument(
        "--sum",
        action="store_true",
        help="Sum values from all files sharing the same date.",
    )
    input_group.add_argument(
        "--period",
        default="year",
        help=(
            "Repeated period to plot. Prefixes of year, month and week "
            "are accepted."
        ),
    )
    point_group.add_argument(
        "--color",
        help="Matplotlib-compatible point colour to use for all series.",
    )
    point_group.add_argument(
        "--min-size",
        type=float,
        default=None,
        help=(
            "Minimum point diameter in points. If only one of min-size or "
            "max-size is given and it crosses the default opposite bound, "
            "the unset bound is pulled to match."
        ),
    )
    point_group.add_argument(
        "--max-size",
        type=float,
        default=None,
        help=(
            "Maximum point diameter in points. If only one of min-size or "
            "max-size is given and it crosses the default opposite bound, "
            "the unset bound is pulled to match."
        ),
    )
    point_group.add_argument(
        "--alpha",
        type=float,
        default=0.75,
        help="Point opacity between 0 and 1.",
    )
    heatmap_group.add_argument(
        "--heatmap",
        action="store_true",
        help="Overlay a smoothed seasonal heatmap behind the points.",
    )
    heatmap_group.add_argument(
        "--heatmap-style",
        default="seasonal",
        help=(
            "Heatmap layout. Prefixes of seasonal and evolving are "
            "accepted. Seasonal collapses all rows into one shared signal; "
            "evolving preserves row-to-row drift."
        ),
    )
    heatmap_group.add_argument(
        "--heatmap-mode",
        default="sum",
        help=(
            "Heatmap weighting. Prefixes of sum, mean and count are "
            "accepted. Sum highlights recurring total magnitude, mean "
            "highlights recurring average intensity, and count highlights "
            "recurring frequency."
        ),
    )
    heatmap_group.add_argument(
        "--heatmap-sigma",
        type=float,
        help=(
            "Horizontal heatmap smoothing width in period units. When "
            "omitted, the default depends on the period: year=10, "
            "month=2, week=0.75."
        ),
    )
    heatmap_group.add_argument(
        "--heatmap-sigma-y",
        type=float,
        default=DEFAULT_HEATMAP_SIGMA_Y,
        help=(
            "Vertical smoothing in row units for evolving heatmaps. Larger "
            "values blend more strongly across nearby rows."
        ),
    )
    heatmap_group.add_argument(
        "--heatmap-alpha",
        type=float,
        default=DEFAULT_HEATMAP_ALPHA,
        help="Heatmap opacity between 0 and 1.",
    )
    guide_group.add_argument(
        "--no-month-lines",
        action="store_true",
        help="Disable the default month-boundary guide lines in year view.",
    )
    point_group.add_argument(
        "-t",
        "--title",
        help="Title for the plot. Defaults to the space-separated filenames.",
    )
    diagnostics_group.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print additional information about the loaded and plotted data.",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> None:  # noqa: CCR001
    """Entry point for the ``tsd-season-plot`` command."""
    parser = create_parser()
    args = parser.parse_args(argv)

    def log(message: str) -> None:
        if args.verbose:
            print(message)

    try:
        period = resolve_prefix(args.period, PERIODS)
    except PrefixMatchError as exc:
        parser.error(str(exc))
    try:
        heatmap_style = resolve_prefix(args.heatmap_style, HEATMAP_STYLES)
    except PrefixMatchError as exc:
        parser.error(str(exc))
    try:
        heatmap_mode = resolve_prefix(args.heatmap_mode, HEATMAP_MODES)
    except PrefixMatchError as exc:
        parser.error(str(exc))

    min_size, max_size = resolve_size_bounds(args.min_size, args.max_size)
    heatmap_sigma_x = resolve_heatmap_sigma(period, args.heatmap_sigma)
    if min_size <= 0 or max_size <= 0:
        parser.error("Point sizes must be positive")
    if min_size > max_size:
        parser.error("--min-size cannot exceed --max-size")
    if not 0 < args.alpha <= 1:
        parser.error("--alpha must be between 0 and 1")
    if heatmap_sigma_x <= 0:
        parser.error("--heatmap-sigma must be positive")
    if args.heatmap_sigma_y <= 0:
        parser.error("--heatmap-sigma-y must be positive")
    if not 0 < args.heatmap_alpha <= 1:
        parser.error("--heatmap-alpha must be between 0 and 1")

    file_specs = [parse_filespec(value) for value in args.files]
    filenames = [name for name, _ in file_specs]
    base_dir = resolve_tsd_dir()
    log(f"Reading data from base directory: {base_dir}")
    log(f"Period resolved to: {period}")
    if args.heatmap:
        log(
            "Heatmap enabled with {} style, {} mode, sigma-x {}, "
            "sigma-y {}.".format(
                heatmap_style,
                heatmap_mode,
                heatmap_sigma_x,
                args.heatmap_sigma_y,
            )
        )

    series = [
        read_series(filename, label, base_dir)
        for filename, label in file_specs
    ]
    total_points = sum(len(item.points) for item in series)
    log(
        "Loaded {} series containing {} points in total.".format(
            len(series), total_points
        )
    )

    if args.sum:
        log("Summing series across files by date.")
        series = [sum_series(series)]
        log(f"Summed series has {len(series[0].points)} aggregated points.")

    if total_points:
        values = [value for item in series for _, value in item.points]
        dates = [date for item in series for date, _ in item.points]
        log(f"Date range: {min(dates)} – {max(dates)}")
        log(f"Value range: {min(values)} – {max(values)}")
    else:
        log("No data points found in the provided files.")

    title = format_title(args, filenames)
    log(f"Plot title: {title}")

    figure = plot_seasonal_series(
        series,
        period=period,
        title=title,
        color=args.color,
        min_size=min_size,
        max_size=max_size,
        alpha=args.alpha,
        heatmap=args.heatmap,
        heatmap_style=heatmap_style,
        heatmap_mode=heatmap_mode,
        heatmap_sigma_x=heatmap_sigma_x,
        heatmap_sigma_y=args.heatmap_sigma_y,
        heatmap_alpha=args.heatmap_alpha,
        show_month_lines=not args.no_month_lines,
    )
    log(f"Generated figure with {len(figure.axes)} axes.")
    plt.show()
