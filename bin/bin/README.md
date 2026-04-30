# Bin Programs

This directory contains the actual executables installed from the repository. Most are small personal utilities, but a few are substantial enough to deserve a quick pointer here.

## Utility groups

* Network and connectivity: [`bandwidth`](bandwidth), [`bandwidth_history.py`](bandwidth_history.py), [`speedtest`](speedtest), [`up-monitor`](up-monitor), [`wifi-qr`](wifi-qr), and [`wifi_summary.py`](wifi_summary.py).
* Time-series and personal data: [`tsd-plot.py`](tsd-plot.py), [`tsd-season-plot.py`](tsd-season-plot.py), [`tsd-time-to-empty.py`](tsd-time-to-empty.py), [`tsd-bicycle`](tsd-bicycle), and [`tsd-sleep`](tsd-sleep).
* File and package comparison: [`compare-file-lists`](compare-file-lists), [`compare-sum-lists`](compare-sum-lists), [`dedup`](dedup), and [`disk-usage.sh`](disk-usage.sh).
* Git helpers: [`gc`](gc), [`git-this-week`](git-this-week), [`git-update-all.sh`](git-update-all.sh), and [`update-gf`](update-gf).
* Window-manager and desktop helpers: [`i3-chromium-browser`](i3-chromium-browser), [`signal-desktop`](signal-desktop), [`journal-edit`](journal-edit), and [`journal-read`](journal-read).

## Notable tools

* [`bandwidth`](bandwidth) is the cleaner current interface for summarizing recorded network quality data, including per-network breakdowns and textual or graphical statistics.
* [`tsd-time-to-empty.py`](tsd-time-to-empty.py) estimates when a decreasing metric will hit zero using smoothing and simulation, which is useful for "how long until empty?" questions on noisy real-world data.
* [`tsd-season-plot.py`](tsd-season-plot.py) projects series into repeated yearly or weekly cycles so recurring patterns become visible at a glance.
* [`tsd-bicycle`](tsd-bicycle) focuses on bicycle mileage logs and can report totals or generate year-over-year plots.
