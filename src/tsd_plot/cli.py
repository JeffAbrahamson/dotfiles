"""Command line interface for plotting TSD data files."""

from __future__ import annotations

import argparse
import datetime as _dt
import statistics
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, Iterable, List, Sequence, Tuple

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import seaborn as sns

BIN_KEYWORDS = {
    "week": 7,
    "month": 30,
    "year": 365,
}

BIN_FUNCTIONS: Dict[str, Callable[[Iterable[float]], float]] = {
    "mean": statistics.fmean,
    "median": statistics.median,
    "sum": sum,
}

PLOT_FORMATS = {"bar", "line", "scatter", "stacked"}

EPOCH = _dt.date(1970, 1, 1)


@dataclass
class SeriesData:
    """Container for a single time series."""

    label: str
    filename: str
    points: List[Tuple[_dt.date, float]]

    def sorted_points(self) -> List[Tuple[_dt.date, float]]:
        return sorted(self.points, key=lambda item: item[0])


class PrefixMatchError(ValueError):
    """Raised when an argument does not match available prefixes."""


def resolve_prefix(value: str, options: Iterable[str]) -> str:
    """Resolve *value* against *options* treating prefixes as valid."""

    normalized = value.lower()
    matches = [option for option in options if option.startswith(normalized)]
    if not matches:
        raise PrefixMatchError(f"Unknown option: {value!r}")
    if len(matches) > 1:
        raise PrefixMatchError(
            f"Ambiguous option {value!r}; matches {', '.join(matches)}"
        )
    return matches[0]


def parse_bin_width(value: str) -> int:
    """Parse the ``--bin-width`` argument."""

    normalized = value.lower()
    for keyword, days in BIN_KEYWORDS.items():
        if keyword.startswith(normalized):
            return days
    try:
        width = int(value)
    except ValueError as exc:  # pragma: no cover - defensive branch
        raise argparse.ArgumentTypeError(
            "Bin width must be an integer or a known keyword"
        ) from exc
    if width <= 0:
        raise argparse.ArgumentTypeError("Bin width must be positive")
    return width


def parse_filespec(value: str) -> Tuple[str, str]:
    """Split *value* into filename and legend label."""

    if ":" in value:
        filename, label = value.split(":", 1)
        if not label:
            label = filename
        return filename, label
    return value, value


def read_series(filename: str, label: str, base_dir: Path) -> SeriesData:
    """Read a single TSD file into a :class:`SeriesData`."""

    path = base_dir / filename
    points: List[Tuple[_dt.date, float]] = []
    with path.open("r", encoding="utf8") as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line:
                continue
            try:
                date_str, value_str = line.split()
                date = _dt.datetime.strptime(date_str, "%Y-%m-%d").date()
                value = float(value_str)
            except ValueError as exc:  # pragma: no cover - defensive branch
                message = f"Could not parse line {line!r} in {path}"
                raise ValueError(message) from exc
            points.append((date, value))
    return SeriesData(label=label, filename=filename, points=points)


def sum_series(series: Sequence[SeriesData]) -> SeriesData:
    """Combine series by summing points with matching dates."""

    aggregated: Dict[_dt.date, float] = {}
    for entry in series:
        for date, value in entry.points:
            aggregated[date] = aggregated.get(date, 0.0) + value
    points = sorted(aggregated.items(), key=lambda item: item[0])
    return SeriesData(label="sum", filename="sum", points=points)


def bin_series(
    series: SeriesData,
    width: int,
    reducer: Callable[[Iterable[float]], float],
) -> SeriesData:
    """Bin series data using *width* days and *reducer* aggregation."""

    grouped: Dict[_dt.date, List[float]] = {}
    for date, value in series.points:
        offset = (date - EPOCH).days % width
        bucket = date - _dt.timedelta(days=offset)
        grouped.setdefault(bucket, []).append(value)
    binned_points = [
        (bucket, reducer(values)) for bucket, values in sorted(grouped.items())
    ]
    return SeriesData(
        label=series.label, filename=series.filename, points=binned_points
    )


def ensure_sorted(series: Sequence[SeriesData]) -> List[SeriesData]:
    """Return a new list of series with points sorted by date."""

    return [
        SeriesData(
            label=item.label,
            filename=item.filename,
            points=item.sorted_points(),
        )
        for item in series
    ]


def compute_std(values: Sequence[float]) -> float:
    """Return the sample standard deviation, ``nan`` if undefined."""

    if len(values) < 2:
        return float("nan")
    try:
        return statistics.stdev(values)
    except statistics.StatisticsError:  # pragma: no cover - defensive
        return float("nan")


def format_title(args: argparse.Namespace, filenames: Sequence[str]) -> str:
    """Determine plot title from arguments and filenames."""

    if args.title:
        return args.title
    return " ".join(filenames)


def prepare_plot_data(
    series_list: Sequence[SeriesData],
) -> Tuple[List[_dt.date], Dict[str, Dict[_dt.date, float]]]:
    """Create lookup tables for plotting grouped bar charts."""

    all_dates = sorted(
        {date for series in series_list for date, _ in series.points}
    )
    value_map: Dict[str, Dict[_dt.date, float]] = {}
    for series in series_list:
        mapping = {date: value for date, value in series.points}
        value_map[series.label] = mapping
    return all_dates, value_map


def plot_series(
    series_list: Sequence[SeriesData],
    plot_format: str,
    y_label: str,
    title: str,
) -> plt.Figure:
    """Plot the prepared series using matplotlib and seaborn."""

    sns.set_theme(style="whitegrid")
    fig, ax = plt.subplots()

    if not series_list:
        ax.set_title(title)
        ax.set_ylabel(y_label)
        ax.set_xlabel("Date")
        return fig

    locator = mdates.AutoDateLocator()
    formatter = mdates.ConciseDateFormatter(locator)
    ax.xaxis.set_major_locator(locator)
    ax.xaxis.set_major_formatter(formatter)

    if plot_format == "stacked" or (
        plot_format == "bar" and len(series_list) > 1
    ):
        all_dates, value_map = prepare_plot_data(series_list)
        date_nums = mdates.date2num(all_dates)
        width = 0.8
        if plot_format == "stacked":
            bottoms = [0.0] * len(all_dates)
            for series in series_list:
                values = [
                    value_map[series.label].get(date, 0.0)
                    for date in all_dates
                ]
                ax.bar(
                    date_nums,
                    values,
                    width=width,
                    bottom=bottoms,
                    label=series.label,
                )
                bottoms = [b + v for b, v in zip(bottoms, values)]
        else:  # grouped bars
            count = len(series_list)
            offsets = [
                width * (idx - (count - 1) / 2) / max(count, 1)
                for idx in range(count)
            ]
            for offset, series in zip(offsets, series_list):
                values = [
                    value_map[series.label].get(date, 0.0)
                    for date in all_dates
                ]
                ax.bar(
                    date_nums + offset,
                    values,
                    width=width / max(count, 1.0),
                    label=series.label,
                )
    elif plot_format == "bar":
        for series in series_list:
            dates = [date for date, _ in series.points]
            date_nums = mdates.date2num(dates)
            values = [value for _, value in series.points]
            ax.bar(date_nums, values, width=0.8, label=series.label)
    elif plot_format == "line":
        for series in series_list:
            dates = [date for date, _ in series.points]
            values = [value for _, value in series.points]
            ax.plot(dates, values, marker="o", label=series.label)
    elif plot_format == "scatter":
        for series in series_list:
            dates = [mdates.date2num(date) for date, _ in series.points]
            values = [value for _, value in series.points]
            ax.scatter(dates, values, label=series.label)
    else:  # pragma: no cover - defensive branch
        raise ValueError(f"Unsupported plot format {plot_format!r}")

    ax.set_title(title)
    ax.set_ylabel(y_label)
    ax.set_xlabel("Date")
    ax.legend()
    fig.autofmt_xdate()
    fig.tight_layout()
    return fig


def create_parser() -> argparse.ArgumentParser:
    """Create the :mod:`argparse` parser for the CLI."""

    parser = argparse.ArgumentParser(
        description="Plot time series data from $HOME/tsd files."
    )
    parser.add_argument(
        "files",
        nargs="+",
        metavar="FILE[:LABEL]",
        help=(
            "Input file names located in $HOME/tsd. Append :LABEL to "
            "customise the legend entry."
        ),
    )
    parser.add_argument(
        "--sum",
        action="store_true",
        help="Sum values from all files sharing the same date.",
    )
    parser.add_argument(
        "--bin",
        action="store_true",
        help="Group dates into bins before plotting.",
    )
    parser.add_argument(
        "--bin-width",
        type=parse_bin_width,
        help=(
            "Number of days per bin. Accepts integers or prefixes of "
            "week (7), month (30) and year (365)."
        ),
    )
    parser.add_argument(
        "--bin-function",
        default="mean",
        help=(
            "Aggregation used within each bin. Accepts prefixes of mean, "
            "median and sum. Defaults to mean."
        ),
    )
    parser.add_argument(
        "--format",
        default="bar",
        help=(
            "Plot style. Prefixes of bar, line, stacked and scatter are "
            "accepted. Defaults to bar."
        ),
    )
    parser.add_argument(
        "--std",
        action="store_true",
        help="Print the sample standard deviation of each plotted series.",
    )
    parser.add_argument(
        "-t",
        "--title",
        help="Title for the plot. Defaults to the space-separated filenames.",
    )
    parser.add_argument(
        "-y",
        "--y-label",
        default="Value",
        help="Label for the Y axis. Defaults to 'Value'.",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print additional information about the loaded and plotted data.",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> None:
    """Entry point for the ``tsd-plot`` command."""

    parser = create_parser()
    args = parser.parse_args(argv)

    def log(message: str) -> None:
        if args.verbose:
            print(message)

    try:
        plot_format = resolve_prefix(args.format, PLOT_FORMATS)
    except PrefixMatchError as exc:
        parser.error(str(exc))

    try:
        reducer_name = resolve_prefix(args.bin_function, BIN_FUNCTIONS)
    except PrefixMatchError as exc:
        parser.error(str(exc))
    reducer = BIN_FUNCTIONS[reducer_name]
    log(f"Plot format resolved to: {plot_format}")
    log(f"Bin function resolved to: {reducer_name}")

    file_specs = [parse_filespec(value) for value in args.files]
    filenames = [name for name, _ in file_specs]
    base_dir = Path.home() / "tsd"
    log(f"Reading data from base directory: {base_dir}")

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
    for item in series:
        log(f"  {item.label} ({item.filename}): {len(item.points)} points")
    all_dates = [date for item in series for date, _ in item.points]
    if all_dates:
        log(f"Date range: {min(all_dates)} – {max(all_dates)}")
    else:
        log("No data points found in the provided files.")

    if args.sum:
        log("Summing series across files by date.")
        series = [sum_series(series)]
        log(f"Summed series has {len(series[0].points)} aggregated points.")

    if args.bin or args.bin_width is not None:
        width = args.bin_width or 7
        log(
            "Binning data with width {} days using {} aggregation.".format(
                width, reducer_name
            )
        )
        series = [bin_series(item, width, reducer) for item in series]
        total_bins = sum(len(item.points) for item in series)
        log(f"Total bins produced: {total_bins}")
        for item in series:
            if item.points:
                log(
                    "  {}: {} bins ({} – {})".format(
                        item.label,
                        len(item.points),
                        item.points[0][0],
                        item.points[-1][0],
                    )
                )
            else:
                log(f"  {item.label}: 0 bins")

    series = ensure_sorted(series)
    total_points = sum(len(item.points) for item in series)
    log(
        "Preparing to plot {} points across {} series.".format(
            total_points, len(series)
        )
    )
    if total_points:
        all_dates = [date for item in series for date, _ in item.points]
        log(f"Final date range: {min(all_dates)} – {max(all_dates)}")

    title = format_title(args, filenames)
    log(f"Plot title: {title}")
    log(f"Y-axis label: {args.y_label}")
    figure = plot_series(series, plot_format, args.y_label, title)
    log(f"Generated figure with {len(figure.axes)} axes.")

    if args.std:
        for item in series:
            std_value = compute_std([value for _, value in item.points])
            print(f"{item.label}: {std_value}")

    plt.show()
