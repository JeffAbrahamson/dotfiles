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
