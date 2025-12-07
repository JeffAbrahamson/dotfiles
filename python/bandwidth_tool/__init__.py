"""Utilities for inspecting recorded bandwidth measurements."""
from __future__ import annotations

import socket
import subprocess
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Mapping, Optional, Sequence

from .tabular import format_table

DEFAULT_LIMIT = 20

UPLOAD_FILE = "speedtest-upload"
DOWNLOAD_FILE = "speedtest-download"
PING_FILE = "speedtest-ping"
SSID_FILE = "speedtest-ssid"


def detect_default_interface() -> Optional[str]:
    """Return the interface used for the default route, or None."""

    try:
        output = subprocess.check_output(
            ["ip", "route", "show", "default"],
            stderr=subprocess.DEVNULL,
            text=True,
        )
    except Exception:
        return None

    for line in output.splitlines():
        line = line.strip()
        if not line.startswith("default "):
            continue
        parts = line.split()
        if "dev" in parts:
            idx = parts.index("dev")
            if idx + 1 < len(parts):
                return parts[idx + 1]
    return None


def detect_wifi_ssid(interface: str) -> Optional[str]:
    """Return SSID for a wifi interface, or None if unavailable."""

    try:
        output = subprocess.check_output(
            ["iwgetid", interface, "--raw"],
            stderr=subprocess.DEVNULL,
            text=True,
        ).strip()
    except Exception:
        return None
    return output or None


def current_network_identifier() -> str:
    """Return SSID if wifi, otherwise default interface name or "unknown"."""

    interface = detect_default_interface()
    if not interface:
        return "unknown"
    ssid = detect_wifi_ssid(interface)
    if ssid:
        return ssid
    return interface


@dataclass
class Measurement:
    """Represents a single measurement row."""

    timestamp: int
    upload: Optional[float] = None
    download: Optional[float] = None
    ping: Optional[float] = None
    ssid: Optional[str] = None

    def as_row(self) -> Sequence[str]:
        dt = datetime.fromtimestamp(self.timestamp).strftime("%Y-%m-%d %H:%M:%S")
        return (
            dt,
            format_number(self.upload),
            format_number(self.download),
            format_number(self.ping),
            self.ssid if self.ssid is not None and self.ssid != "" else "-",
        )


def format_number(value: Optional[float]) -> str:
    if value is None:
        return "-"
    return f"{value:0.2f}"


def default_data_directory() -> Path:
    hostname = socket.gethostname().split(".")[0]
    return Path.home() / "data" / "hosts" / hostname


def _ensure_files(directory: Path) -> Mapping[str, Path]:
    paths = {
        "upload": directory / UPLOAD_FILE,
        "download": directory / DOWNLOAD_FILE,
        "ping": directory / PING_FILE,
        "ssid": directory / SSID_FILE,
    }
    missing = [str(path) for path in paths.values() if not path.is_file()]
    if missing:
        raise FileNotFoundError(f"Missing file(s): {', '.join(missing)}")
    return paths


def _parse_numeric_file(path: Path) -> Dict[int, float]:
    data: Dict[int, float] = {}
    with path.open("r", encoding="utf-8") as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line:
                continue
            timestamp_str, _, value_str = line.partition(" ")
            if not value_str:
                continue
            timestamp = int(timestamp_str)
            value = float(value_str.strip())
            data[timestamp] = value
    return data


def _parse_text_file(path: Path) -> Dict[int, str]:
    data: Dict[int, str] = {}
    with path.open("r", encoding="utf-8") as handle:
        for raw_line in handle:
            line = raw_line.rstrip("\n")
            if not line:
                continue
            timestamp_str, _, value = line.partition(" ")
            timestamp = int(timestamp_str)
            data[timestamp] = value.lstrip()
    return data


def load_measurements(directory: Path) -> List[Measurement]:
    paths = _ensure_files(directory)
    uploads = _parse_numeric_file(paths["upload"])
    downloads = _parse_numeric_file(paths["download"])
    pings = _parse_numeric_file(paths["ping"])
    ssids = _parse_text_file(paths["ssid"])

    timestamps = sorted({*uploads.keys(), *downloads.keys(), *pings.keys(), *ssids.keys()})
    measurements: List[Measurement] = []
    for ts in timestamps:
        measurements.append(
            Measurement(
                timestamp=ts,
                upload=uploads.get(ts),
                download=downloads.get(ts),
                ping=pings.get(ts),
                ssid=ssids.get(ts),
            )
        )
    return measurements


def limit_measurements(measurements: Sequence[Measurement], limit: int) -> List[Measurement]:
    if limit <= 0:
        return []
    return list(measurements[-limit:])


def render_table(measurements: Iterable[Measurement]) -> str:
    rows = [measurement.as_row() for measurement in measurements]
    headers = ("datetime", "upload MiBps", "download MiBps", "ping ms", "ssid")
    return format_table(headers, rows, colalign=("left", "right", "right", "right", "left"))


def _collect_values(measurements: Iterable[Measurement], attribute: str) -> List[float]:
    values: List[float] = []
    for measurement in measurements:
        value = getattr(measurement, attribute)
        if value is not None:
            values.append(value)
    return values


def _determine_edges(values: Sequence[float], bins: int = 10) -> List[float]:
    if not values:
        return [0.0, 1.0]
    minimum = min(values)
    maximum = max(values)
    if minimum == maximum:
        width = max(abs(minimum) * 0.1, 1.0)
        return [minimum - width, minimum + width]
    bins = max(1, bins)
    step = (maximum - minimum) / bins
    edges = [minimum + step * i for i in range(bins)]
    edges.append(maximum)
    edges[0] = minimum
    edges[-1] = maximum
    return edges


def _histogram_from_edges(values: Sequence[float], edges: Sequence[float]) -> List[int]:
    counts = [0 for _ in range(len(edges) - 1)]
    if not values:
        return counts
    for value in values:
        if value <= edges[0]:
            index = 0
        elif value >= edges[-1]:
            index = len(counts) - 1
        else:
            for idx in range(len(edges) - 1):
                if edges[idx] <= value < edges[idx + 1]:
                    index = idx
                    break
            else:  # pragma: no cover - defensive programming
                index = len(counts) - 1
        counts[index] += 1
    return counts


def _bin_centers(edges: Sequence[float]) -> List[float]:
    return [(edges[i] + edges[i + 1]) / 2 for i in range(len(edges) - 1)]


def _format_range(start: float, end: float, is_last: bool) -> str:
    right = "]" if is_last else ")"
    return f"[{start:7.2f}, {end:7.2f}{right}"


def _bar(count: int, max_count: int, width: int, *, reverse: bool = False) -> str:
    if max_count <= 0 or count <= 0:
        bar = ""
    else:
        scaled = max(1, round(count / max_count * width))
        bar = "█" * min(width, scaled)
    if reverse:
        return bar.rjust(width)
    return bar.ljust(width)


def _render_violin_text(edges: Sequence[float], upload_counts: Sequence[int], download_counts: Sequence[int]) -> List[str]:
    width = 16
    lines = ["Upload/Download speeds (MiBps)"]
    lines.append("upload".rjust(width) + " │ " + "download".ljust(width))
    max_count = max([*upload_counts, *download_counts, 0])
    for idx, (start, end) in enumerate(zip(edges[:-1], edges[1:])):
        label = _format_range(start, end, idx == len(edges) - 2)
        label = label.ljust(23)
        left = _bar(upload_counts[idx], max_count, width, reverse=True)
        right = _bar(download_counts[idx], max_count, width)
        lines.append(f"{label} {left}│{right}")
    return lines


def _render_ping_text(edges: Sequence[float], counts: Sequence[int]) -> List[str]:
    width = 32
    lines = ["Ping times (ms)"]
    max_count = max([*counts, 0])
    for idx, (start, end) in enumerate(zip(edges[:-1], edges[1:])):
        label = _format_range(start, end, idx == len(edges) - 2)
        label = label.ljust(23)
        bar = _bar(counts[idx], max_count, width)
        lines.append(f"{label} {bar}")
    return lines


def render_stats_text(measurements: Sequence[Measurement], bins: int = 10) -> str:
    uploads = _collect_values(measurements, "upload")
    downloads = _collect_values(measurements, "download")
    pings = _collect_values(measurements, "ping")

    lines: List[str] = []

    if uploads or downloads:
        edges = _determine_edges([*uploads, *downloads], bins=bins)
        upload_counts = _histogram_from_edges(uploads, edges)
        download_counts = _histogram_from_edges(downloads, edges)
        lines.extend(_render_violin_text(edges, upload_counts, download_counts))
    else:
        lines.append("No upload/download data available.")

    lines.append("")

    if pings:
        ping_edges = _determine_edges(pings, bins=bins)
        ping_counts = _histogram_from_edges(pings, ping_edges)
        lines.extend(_render_ping_text(ping_edges, ping_counts))
    else:
        lines.append("No ping data available.")

    return "\n".join(lines)


def render_stats_graphical(measurements: Sequence[Measurement], bins: int = 10) -> None:
    try:
        import matplotlib.pyplot as plt  # type: ignore
    except ImportError as exc:  # pragma: no cover - depends on optional dependency
        raise RuntimeError("Matplotlib is required for graphical statistics") from exc

    uploads = _collect_values(measurements, "upload")
    downloads = _collect_values(measurements, "download")
    pings = _collect_values(measurements, "ping")

    fig, (ax_speed, ax_ping) = plt.subplots(2, 1, figsize=(8, 8))

    edges = _determine_edges([*uploads, *downloads], bins=bins)
    upload_counts = _histogram_from_edges(uploads, edges)
    download_counts = _histogram_from_edges(downloads, edges)
    centers = _bin_centers(edges)
    heights = [edges[i + 1] - edges[i] for i in range(len(edges) - 1)]

    ax_speed.barh(centers, upload_counts, height=heights, align="center", color="tab:blue", label="Upload")
    ax_speed.barh(centers, [-count for count in download_counts], height=heights, align="center", color="tab:orange", label="Download")
    ax_speed.axvline(0, color="black", linewidth=0.8)
    ax_speed.set_xlabel("Sample count")
    ax_speed.set_ylabel("MiBps")
    ax_speed.set_title("Upload/Download distribution")
    ax_speed.legend()

    ping_edges = _determine_edges(pings, bins=bins)
    ping_counts = _histogram_from_edges(pings, ping_edges)
    ping_centers = _bin_centers(ping_edges)
    widths = [ping_edges[i + 1] - ping_edges[i] for i in range(len(ping_edges) - 1)]

    ax_ping.bar(ping_centers, ping_counts, width=widths, color="tab:green")
    ax_ping.set_xlabel("Ping (ms)")
    ax_ping.set_ylabel("Sample count")
    ax_ping.set_title("Ping distribution")

    plt.tight_layout()
    plt.show()


def render_stats(measurements: Sequence[Measurement], *, text: bool, bins: int = 10) -> Optional[str]:
    if text:
        return render_stats_text(measurements, bins=bins)
    render_stats_graphical(measurements, bins=bins)
    return None


__all__ = [
    "DEFAULT_LIMIT",
    "Measurement",
    "default_data_directory",
    "limit_measurements",
    "load_measurements",
    "render_stats",
    "render_stats_graphical",
    "render_stats_text",
    "render_table",
    "detect_default_interface",
    "detect_wifi_ssid",
    "current_network_identifier",
]
