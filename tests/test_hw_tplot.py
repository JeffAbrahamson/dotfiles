import importlib.machinery
import importlib.util
from pathlib import Path

import pytest


def load_hw_tplot():
    path = Path(__file__).parents[1] / "bin" / "bin" / "hw-tplot"
    loader = importlib.machinery.SourceFileLoader("hw_tplot", str(path))
    spec = importlib.util.spec_from_loader("hw_tplot", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


HW_TPLOT = load_hw_tplot()


def test_parse_time_relative_defaults_to_past():
    now = 1_800_000_000

    assert HW_TPLOT.parse_time("2d", now=now) == now - 2 * 24 * 60 * 60
    assert HW_TPLOT.parse_time("-2d", now=now) == now - 2 * 24 * 60 * 60
    assert HW_TPLOT.parse_time("+2d", now=now) == now + 2 * 24 * 60 * 60


def test_parse_time_accepts_iso_timestamp_with_timezone():
    timestamp = HW_TPLOT.parse_time("2026-07-10T00:00:00+00:00")

    assert timestamp == pytest.approx(1_783_641_600)


def test_select_temperature_files_accepts_display_name():
    filenames = [
        Path("/tmp/temperature-amdgpu-edge"),
        Path("/tmp/temperature-k10temp-tctl"),
    ]

    assert HW_TPLOT.select_temperature_files(filenames, ["k10temp-tctl"]) == [
        Path("/tmp/temperature-k10temp-tctl")
    ]


def test_select_temperature_files_rejects_unknown_series():
    filenames = [Path("/tmp/temperature-k10temp-tctl")]

    with pytest.raises(ValueError, match="unknown temperature series"):
        HW_TPLOT.select_temperature_files(filenames, ["nope"])


@pytest.mark.parametrize(
    ("name", "description"),
    [
        ("k10temp-tctl", "CPU control temperature"),
        ("amdgpu-edge", "GPU edge temperature"),
        ("nvme-composite", "NVMe SSD composite temperature"),
        ("acpitz-temp1", "ACPI thermal zone"),
        ("mt7921_phy0-temp1", "Wi-Fi adapter temperature"),
    ],
)
def test_describe_series(name, description):
    assert HW_TPLOT.describe_series(name) == description


def test_recent_peak_uses_recent_window():
    points = [
        (100.0, 99.0),
        (200.0, 40.0),
        (300.0, 55.0),
        (400.0, 50.0),
    ]

    assert HW_TPLOT.recent_peak(points, 150) == (300.0, 55.0)


def test_recent_peak_window_uses_quarter_of_short_visible_range():
    series = {"cpu": [(0.0, 50.0), (1200.0, 60.0)]}

    assert HW_TPLOT.recent_peak_window(series, 24 * 60 * 60) == 60 * 60


def test_recent_peak_window_caps_long_visible_range():
    series = {"cpu": [(0.0, 50.0), (10 * 24 * 60 * 60, 60.0)]}

    assert HW_TPLOT.recent_peak_window(series, 24 * 60 * 60) == 24 * 60 * 60


def test_shutdown_times_returns_last_gap_start_times():
    series = {
        "cpu": [
            (100.0, 50.0),
            (200.0, 51.0),
            (500.0, 80.0),
            (900.0, 70.0),
        ],
        "gpu": [
            (100.0, 40.0),
            (200.0, 41.0),
            (500.0, 65.0),
            (900.0, 60.0),
        ],
    }

    assert HW_TPLOT.shutdown_times(series, 250, count=2) == [200.0, 500.0]


def test_temperatures_at_uses_friendly_series_labels():
    series = {
        "k10temp-tctl": [(100.0, 50.0), (200.0, 55.0)],
        "amdgpu-edge": [(100.0, 45.0), (300.0, 65.0)],
    }

    assert HW_TPLOT.temperatures_at(series, 250.0) == [
        ("k10temp-tctl (CPU control temperature)", 55.0),
        ("amdgpu-edge (GPU edge temperature)", 45.0),
    ]


def test_print_run_summary_reports_hidpi(capsys):
    series = {
        "k10temp-tctl": [(100.0, 50.0), (400.0, 80.0)],
        "amdgpu-edge": [(100.0, 45.0), (400.0, 65.0)],
    }

    HW_TPLOT.print_run_summary(series, 250, detected_hidpi=True, hidpi=True)

    output = capsys.readouterr().out
    assert "Temperature checkpoints:" in output
    assert "shutdown before time-series gap: 1970-01-01" in output
    assert ("    * k10temp-tctl (CPU control temperature)   50.0 C") in output
    assert ("    * amdgpu-edge (GPU edge temperature)       45.0 C") in output
    assert "current last recorded time: 1970-01-01" in output
    assert (
        "HiDPI display detected: yes; larger plot fonts applied: yes" in output
    )


def test_print_run_summary_reports_unknown_hidpi_detection(capsys):
    series = {"k10temp-tctl": [(100.0, 50.0)]}

    HW_TPLOT.print_run_summary(series, 250, detected_hidpi=None, hidpi=False)

    output = capsys.readouterr().out
    assert (
        "HiDPI display detected: unknown; larger plot fonts applied: no"
        in output
    )


def test_output_is_hidpi_when_sway_scale_is_greater_than_one():
    output = {"active": True, "scale": 2.0, "current_mode": {}}

    assert HW_TPLOT.output_is_hidpi(output)


def test_output_is_hidpi_for_4k_mode_even_at_scale_one():
    output = {
        "active": True,
        "scale": 1.0,
        "current_mode": {"width": 3840, "height": 2160},
    }

    assert HW_TPLOT.output_is_hidpi(output)


def test_output_is_hidpi_from_physical_dpi():
    output = {
        "active": True,
        "scale": 1.0,
        "current_mode": {"width": 3840, "height": 2160},
        "physical_width": 344,
        "physical_height": 194,
    }

    assert HW_TPLOT.output_is_hidpi(output)


def test_output_is_not_hidpi_for_inactive_outputs():
    output = {
        "active": False,
        "scale": 2.0,
        "current_mode": {"width": 5120, "height": 2880},
    }

    assert not HW_TPLOT.output_is_hidpi(output)
