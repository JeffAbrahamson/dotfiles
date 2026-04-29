"""Tests for the :mod:`tsd_plot.seasonal` module."""

import datetime as dt

import pytest


def _load_dependencies():
    try:
        import matplotlib
    except ModuleNotFoundError as exc:  # pragma: no cover - dependency guard
        pytest.skip(
            "matplotlib is required for these tests",
            allow_module_level=True,
        )
        raise exc

    matplotlib.use("Agg")

    try:
        import matplotlib.pyplot as plt  # type: ignore
    except ModuleNotFoundError as exc:  # pragma: no cover - dependency guard
        pytest.skip(
            "matplotlib is required for these tests",
            allow_module_level=True,
        )
        raise exc

    try:
        import seaborn  # noqa: F401  # pragma: no cover - dependency guard
    except ModuleNotFoundError as exc:  # pragma: no cover - dependency guard
        pytest.skip(
            "seaborn is required for these tests",
            allow_module_level=True,
        )
        raise exc

    return plt


plt = _load_dependencies()

from tsd_plot import seasonal  # noqa: E402


@pytest.fixture(autouse=True)
def _no_show(monkeypatch):
    """Prevent matplotlib from opening GUI windows during tests."""
    monkeypatch.setattr(plt, "show", lambda: None)


def test_project_series_to_year_period():
    """Year projection should preserve day-of-year positions."""
    series = seasonal.SeriesData(
        label="demo",
        filename="demo",
        points=[
            (dt.date(2024, 1, 1), 1.0),
            (dt.date(2024, 12, 31), 2.0),
        ],
    )

    projected = seasonal.project_series_to_period(series, "year")

    assert projected == [("2024", 0.0, 1.0), ("2024", 365.0, 2.0)]


def test_size_map_uses_diameter_bounds():
    """Point sizes should respect the requested diameter bounds."""
    sizes = seasonal.size_map([1.0, 3.0, 5.0], 4.0, 10.0)

    assert sizes[0] == pytest.approx(16.0)
    assert sizes[-1] == pytest.approx(100.0)


def test_resolve_size_bounds_promotes_single_override():
    """Single-sided size overrides should expand the opposite bound."""
    assert seasonal.resolve_size_bounds(20.0, None) == (20.0, 20.0)
    assert seasonal.resolve_size_bounds(None, 4.0) == (4.0, 4.0)
    assert seasonal.resolve_size_bounds(7.0, None) == (7.0, 16.0)
    assert seasonal.resolve_size_bounds(None, 12.0) == (5.0, 12.0)


def test_compute_heatmap_signal_finds_recurring_peak():
    """Heatmap smoothing should highlight repeated seasonal positions."""
    projected = [
        ("2024", 10.0, 4.0),
        ("2025", 10.0, 5.0),
        ("2024", 25.0, 1.0),
    ]

    signal = seasonal.compute_heatmap_signal(
        projected,
        span=31,
        mode="sum",
        sigma=1.5,
    )

    assert signal[10] == pytest.approx(1.0)
    assert signal[10] > signal[25]


def test_compute_heatmap_signal_mean_mode_uses_average_value():
    """Mean mode should not let frequent low values dominate a hot zone."""
    projected = [
        ("2024", 3.0, 2.0),
        ("2025", 3.0, 2.0),
        ("2024", 5.0, 9.0),
    ]

    signal = seasonal.compute_heatmap_signal(
        projected,
        span=7,
        mode="mean",
        sigma=0.5,
    )

    assert signal[5] > signal[3]


def test_compute_evolving_heatmap_preserves_row_shift():
    """Evolving heatmaps should retain a gradual seasonal shift."""
    projected = [
        ("2025", 8.0, 3.0),
        ("2024", 10.0, 3.0),
        ("2023", 12.0, 3.0),
    ]
    row_positions = {"2025": 0, "2024": 1, "2023": 2}

    image = seasonal.compute_evolving_heatmap(
        projected,
        row_positions=row_positions,
        row_count=3,
        span=20,
        mode="sum",
        sigma_x=0.8,
        sigma_y=0.35,
    )

    peaks = [row.index(max(row)) for row in image]
    assert peaks == [8, 10, 12]


def test_plot_seasonal_series_adds_month_lines_for_year_view():
    """Year plots should include the default month-boundary guides."""
    series = [
        seasonal.SeriesData(
            label="demo",
            filename="demo",
            points=[
                (dt.date(2023, 1, 1), 1.0),
                (dt.date(2024, 2, 29), 3.0),
            ],
        )
    ]

    figure = seasonal.plot_seasonal_series(
        series,
        period="year",
        title="Seasonal",
        color="tab:blue",
        min_size=4.0,
        max_size=8.0,
        alpha=0.75,
        heatmap=True,
        heatmap_style="seasonal",
        heatmap_mode="sum",
        heatmap_sigma_x=10.0,
        heatmap_sigma_y=0.75,
        heatmap_alpha=0.35,
        show_month_lines=True,
    )

    axis = figure.axes[0]
    assert axis.get_yticklabels()[0].get_text() == "2024"
    assert len(axis.collections) == 1
    assert len(axis.images) == 1
    assert len(axis.lines) == 11


def test_main_reads_files_and_respects_no_month_lines(
    tmp_path, monkeypatch, capsys
):
    """CLI should load data and emit useful verbose information."""
    data_dir = tmp_path / "tsd"
    data_dir.mkdir()
    (data_dir / "rain").write_text(
        "\n".join(
            [
                "2024-01-01\t1",
                "2024-02-29\t3",
                "2025-01-10\t2",
            ]
        ),
        encoding="utf8",
    )

    monkeypatch.setenv("TSD", str(data_dir))
    monkeypatch.delenv("TSD_DIR", raising=False)

    seasonal.main(
        [
            "rain",
            "--period",
            "ye",
            "--color",
            "black",
            "--min-size",
            "6",
            "--max-size",
            "12",
            "--alpha",
            "0.6",
            "--heatmap",
            "--heatmap-style",
            "evo",
            "--heatmap-mode",
            "co",
            "--heatmap-sigma",
            "1.2",
            "--heatmap-sigma-y",
            "0.5",
            "--heatmap-alpha",
            "0.4",
            "--no-month-lines",
            "--verbose",
        ]
    )

    captured = capsys.readouterr()
    assert "Period resolved to: year" in captured.out
    assert (
        "Heatmap enabled with evolving style, count mode, sigma-x 1.2, "
        "sigma-y 0.5." in captured.out
    )
    assert "Loaded 1 series containing 3 points in total." in captured.out
    assert "Value range: 1.0 – 3.0" in captured.out


def test_main_reads_from_tsd_environment_variable(
    tmp_path, monkeypatch, capsys
):
    """CLI should prefer the explicit TSD environment variable."""
    data_dir = tmp_path / "custom-tsd"
    data_dir.mkdir()
    (data_dir / "rain").write_text("2024-01-01\t1\n", encoding="utf8")

    monkeypatch.setenv("TSD", str(data_dir))
    monkeypatch.delenv("TSD_DIR", raising=False)

    seasonal.main(["rain", "--verbose"])

    captured = capsys.readouterr()
    assert f"Reading data from base directory: {data_dir}" in captured.out


def test_main_accepts_only_min_size(tmp_path, monkeypatch):
    """CLI should accept a lone minimum size above the default maximum."""
    data_dir = tmp_path / "tsd"
    data_dir.mkdir()
    (data_dir / "rain").write_text("2024-01-01\t1\n", encoding="utf8")

    monkeypatch.setenv("TSD", str(data_dir))
    monkeypatch.delenv("TSD_DIR", raising=False)

    seasonal.main(["rain", "--min-size", "20"])


def test_help_mentions_overview_groups_and_defaults(capsys):
    """Help output should summarize intent, groups, and default values."""
    with pytest.raises(SystemExit):
        seasonal.main(["--help"])

    captured = capsys.readouterr()
    assert "Plot repeated-season views of TSD time series" in captured.out
    assert "input and grouping:" in captured.out
    assert "point appearance:" in captured.out
    assert "heatmap overlay:" in captured.out
    assert "reference guides:" in captured.out
    assert "diagnostics:" in captured.out
    assert "(default: year)" in captured.out
    assert "(default: 0.75)" in captured.out
    assert "--heatmap-style HEATMAP_STYLE" in captured.out
    assert "seasonal)" in captured.out


def test_month_and_week_axes_skip_month_lines():
    """Non-year periods should not add month-boundary guide lines."""
    series = [
        seasonal.SeriesData(
            label="demo",
            filename="demo",
            points=[
                (dt.date(2024, 3, 1), 1.0),
                (dt.date(2024, 3, 2), 2.0),
            ],
        )
    ]

    month_figure = seasonal.plot_seasonal_series(
        series,
        period="month",
        title="By month",
        color=None,
        min_size=5.0,
        max_size=10.0,
        alpha=0.75,
        heatmap=False,
        heatmap_style="seasonal",
        heatmap_mode="sum",
        heatmap_sigma_x=2.0,
        heatmap_sigma_y=0.75,
        heatmap_alpha=0.35,
        show_month_lines=True,
    )
    week_figure = seasonal.plot_seasonal_series(
        series,
        period="week",
        title="By week",
        color=None,
        min_size=5.0,
        max_size=10.0,
        alpha=0.75,
        heatmap=False,
        heatmap_style="seasonal",
        heatmap_mode="sum",
        heatmap_sigma_x=0.75,
        heatmap_sigma_y=0.75,
        heatmap_alpha=0.35,
        show_month_lines=True,
    )

    assert len(month_figure.axes[0].lines) == 0
    assert len(week_figure.axes[0].lines) == 0
