from __future__ import annotations

import os
import subprocess
import sys
from datetime import datetime
from pathlib import Path

import pytest

# Make repo python utilities importable when tests run directly.
PROJECT_ROOT = Path(__file__).resolve().parents[1]
PYTHON_DIR = PROJECT_ROOT / "python"
if str(PYTHON_DIR) not in sys.path:
    sys.path.insert(0, str(PYTHON_DIR))

import bandwidth_tool  # noqa: E402
from bandwidth_tool import (  # noqa: E402
    current_network_identifier,
    detect_default_interface,
    detect_wifi_ssid,
    limit_measurements,
    load_measurements,
    render_stats,
    render_stats_text,
    render_table,
)


@pytest.fixture()
def sample_data(tmp_path: Path) -> Path:
    timestamps = [1_700_000_000, 1_700_000_600, 1_700_001_200]
    uploads = [100.0, 110.0, 120.0]
    downloads = [200.0, 210.0, 220.0]
    pings = [10.0, 11.0, 12.0]
    ssids = ["Office WiFi", "Office WiFi", "Guest Network"]

    def write_file(name: str, values: list[str]) -> None:
        path = tmp_path / name
        path.write_text("\n".join(values) + "\n", encoding="utf-8")

    write_file(
        "speedtest-upload",
        [f"{ts} {value}" for ts, value in zip(timestamps, uploads)],
    )
    write_file(
        "speedtest-download",
        [f"{ts} {value}" for ts, value in zip(timestamps, downloads)],
    )
    write_file(
        "speedtest-ping",
        [f"{ts} {value}" for ts, value in zip(timestamps, pings)],
    )
    write_file(
        "speedtest-ssid",
        [f"{ts} {ssid}" for ts, ssid in zip(timestamps, ssids)],
    )
    return tmp_path


def test_detect_default_interface_parses_output(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    def fake_check_output(
        cmd: list[str], *, stderr: object, text: bool
    ) -> str:
        assert cmd == ["ip", "route", "show", "default"]
        return "default via 192.168.1.1 dev wlp3s0 proto dhcp metric 600\n"

    monkeypatch.setattr(subprocess, "check_output", fake_check_output)

    assert detect_default_interface() == "wlp3s0"


def test_detect_wifi_ssid(monkeypatch: pytest.MonkeyPatch) -> None:
    def fake_check_output(
        cmd: list[str], *, stderr: object, text: bool
    ) -> str:
        assert cmd == ["iwgetid", "wlp3s0", "--raw"]
        return "OfficeWifi\n"

    monkeypatch.setattr(subprocess, "check_output", fake_check_output)

    assert detect_wifi_ssid("wlp3s0") == "OfficeWifi"


def test_current_network_identifier_prefers_ssid(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(
        bandwidth_tool, "detect_default_interface", lambda: "wlp3s0"
    )
    monkeypatch.setattr(
        bandwidth_tool, "detect_wifi_ssid", lambda interface: "OfficeWifi"
    )

    assert current_network_identifier() == "OfficeWifi"


def test_current_network_identifier_falls_back_to_interface(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(
        bandwidth_tool, "detect_default_interface", lambda: "enp0s1"
    )
    monkeypatch.setattr(
        bandwidth_tool, "detect_wifi_ssid", lambda interface: None
    )

    assert current_network_identifier() == "enp0s1"


def test_current_network_identifier_unknown(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.setattr(
        bandwidth_tool, "detect_default_interface", lambda: None
    )

    assert current_network_identifier() == "unknown"


def test_render_table_with_limit(sample_data: Path) -> None:
    measurements = load_measurements(sample_data)
    limited = limit_measurements(measurements, 2)
    table = render_table(limited)

    lines = table.splitlines()
    assert lines[0].startswith("datetime")
    # Ensure we only rendered the most recent two timestamps.
    expected_dates = [
        datetime.fromtimestamp(ts).strftime("%Y-%m-%d %H:%M:%S")
        for ts in (1_700_000_600, 1_700_001_200)
    ]
    actual_dates = [line[:19] for line in lines[2:]]
    assert actual_dates == expected_dates
    # Numbers should be formatted with two decimals.
    body_lines = lines[2:]
    assert any("210.00" in line for line in body_lines)
    assert any("12.00" in line for line in body_lines)


def test_cli_outputs_table(sample_data: Path) -> None:
    env = os.environ.copy()
    env["PYTHONPATH"] = str(PYTHON_DIR)
    script = PROJECT_ROOT / "bin" / "bin" / "bandwidth"
    result = subprocess.run(
        [str(script), "-n", "1", str(sample_data)],
        check=False,
        capture_output=True,
        text=True,
        env=env,
    )
    assert result.returncode == 0
    assert "datetime" in result.stdout.splitlines()[0]
    assert "Guest" in result.stdout


def test_cli_missing_file_error(tmp_path: Path) -> None:
    # Only create a subset of the required files.
    (tmp_path / "speedtest-upload").write_text(
        "1700000000 10.0\n", encoding="utf-8"
    )
    env = os.environ.copy()
    env["PYTHONPATH"] = str(PYTHON_DIR)
    script = PROJECT_ROOT / "bin" / "bin" / "bandwidth"
    result = subprocess.run(
        [str(script), str(tmp_path)],
        check=False,
        capture_output=True,
        text=True,
        env=env,
    )
    assert result.returncode == 1
    assert "Missing file" in result.stderr


def test_render_stats_text(sample_data: Path) -> None:
    measurements = load_measurements(sample_data)
    limited = limit_measurements(measurements, 3)
    stats_output = render_stats_text(limited)
    assert "Upload/Download speeds" in stats_output
    assert "Ping times" in stats_output


def test_cli_stats_text(sample_data: Path) -> None:
    env = os.environ.copy()
    env["PYTHONPATH"] = str(PYTHON_DIR)
    script = PROJECT_ROOT / "bin" / "bin" / "bandwidth"
    result = subprocess.run(
        [str(script), "--stats", "--text", "-n", "2", str(sample_data)],
        check=False,
        capture_output=True,
        text=True,
        env=env,
    )
    assert result.returncode == 0
    assert "Upload/Download speeds" in result.stdout
    assert "Ping times" in result.stdout


def test_render_stats_requires_matplotlib(sample_data: Path) -> None:
    measurements = load_measurements(sample_data)
    limited = limit_measurements(measurements, 1)
    with pytest.raises(RuntimeError):
        render_stats(limited, text=False)
