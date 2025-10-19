"""Tests for the :mod:`tsd_plot` package."""

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

from tsd_plot import cli, main  # noqa: E402


@pytest.fixture(autouse=True)
def _no_show(monkeypatch):
    """Prevent matplotlib from opening GUI windows during tests."""

    monkeypatch.setattr(plt, "show", lambda: None)


def test_parse_bin_width_keywords():
    assert cli.parse_bin_width("week") == 7
    assert cli.parse_bin_width("mo") == 30
    assert cli.parse_bin_width("year") == 365


def test_resolve_prefix_errors():
    with pytest.raises(cli.PrefixMatchError):
        cli.resolve_prefix("x", {"bar", "line"})
    with pytest.raises(cli.PrefixMatchError):
        cli.resolve_prefix("s", {"sum", "scatter"})


def test_sum_and_bin_series(tmp_path, monkeypatch, capsys):
    data_dir = tmp_path / "tsd"
    data_dir.mkdir()
    (data_dir / "reading").write_text(
        "\n".join(
            [
                "2024-01-01\t1",
                "2024-01-02\t2",
                "2024-01-08\t3",
            ]
        ),
        encoding="utf8",
    )
    (data_dir / "exercise").write_text(
        "\n".join(
            [
                "2024-01-01\t4",
                "2024-01-03\t5",
                "2024-01-08\t6",
            ]
        ),
        encoding="utf8",
    )

    monkeypatch.setattr(cli.Path, "home", lambda: tmp_path)

    main(
        [
            "reading",
            "exercise:fitness",
            "--sum",
            "--bin",
            "--bin-width",
            "week",
            "--bin-function",
            "su",
            "--std",
            "--format",
            "bar",
            "--verbose",
        ]
    )

    captured = capsys.readouterr()
    assert "Loaded 2 series containing 6 points in total." in captured.out
    assert "Summing series across files by date." in captured.out
    assert (
        "Binning data with width 7 days using sum aggregation." in captured.out
    )
    assert "Total bins produced: 2" in captured.out
    std_lines = [
        line for line in captured.out.splitlines() if line.startswith("sum:")
    ]
    assert std_lines
    std_value = float(std_lines[0].split(":", 1)[1].strip())
    assert std_value == pytest.approx(2.12132034)


def test_plot_series_formats(tmp_path, monkeypatch):
    base_dir = tmp_path / "tsd"
    base_dir.mkdir()
    file_path = base_dir / "data"
    file_path.write_text("2024-05-01\t5\n2024-05-02\t7\n", encoding="utf8")
    monkeypatch.setattr(cli.Path, "home", lambda: tmp_path)

    series = [
        cli.read_series("data", "data", base_dir),
    ]
    bar_fig = cli.plot_series(series, "bar", "Value", "Title")
    assert bar_fig.axes[0].get_ylabel() == "Value"

    line_fig = cli.plot_series(series, "line", "Label", "Line Title")
    assert line_fig.axes[0].get_title() == "Line Title"

    scatter_fig = cli.plot_series(series, "scatter", "Y", "Scatter")
    assert scatter_fig.axes[0].has_data()


def test_compute_std_nan():
    assert str(cli.compute_std([1.0])) == "nan"
    assert cli.compute_std([1.0, 2.0, 3.0]) == pytest.approx(1.0)


def test_bin_series_reducer():
    series = cli.SeriesData(
        label="demo",
        filename="demo",
        points=[
            (dt.date(2024, 1, 1), 1.0),
            (dt.date(2024, 1, 2), 2.0),
            (dt.date(2024, 1, 8), 4.0),
        ],
    )
    binned = cli.bin_series(series, 7, cli.BIN_FUNCTIONS["mean"])
    assert len(binned.points) == 2
    assert binned.points[0][1] == pytest.approx(1.5)
    assert binned.points[1][1] == pytest.approx(4.0)
