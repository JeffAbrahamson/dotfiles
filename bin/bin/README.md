# Bin Programs

This directory contains the actual executables installed from the repository. Most are small personal utilities, but a few are substantial enough to deserve a quick pointer here.

## Utility groups

* Network and connectivity: [`bandwidth`](bandwidth), [`bandwidth_history.py`](bandwidth_history.py), [`speedtest`](speedtest), [`up-monitor`](up-monitor), [`wifi-qr`](wifi-qr), and [`wifi_summary.py`](wifi_summary.py).
* Time-series and personal data: [`tsd-bicycle`](tsd-bicycle) and [`tsd-sleep`](tsd-sleep); general tsd plotting commands (`tsd-plot`, `tsd-season-plot`) are installed with the `tsd` package.
* File and package comparison: [`compare-file-lists`](compare-file-lists), [`compare-sum-lists`](compare-sum-lists), [`dedup`](dedup), and [`disk-usage.sh`](disk-usage.sh).
* Git helpers: [`gc`](gc), [`git-this-week`](git-this-week), [`git-update-all.sh`](git-update-all.sh), and [`update-gf`](update-gf).
* Window-manager and desktop helpers: [`i3-chromium-browser`](i3-chromium-browser), [`signal-desktop`](signal-desktop), [`journal-edit`](journal-edit), and [`journal-read`](journal-read).

## Notable tools

* [`bandwidth`](bandwidth) is the cleaner current interface for summarizing recorded network quality data, including per-network breakdowns and textual or graphical statistics.
* [`tsd-bicycle`](tsd-bicycle) focuses on bicycle mileage logs and can report totals or generate year-over-year plots.
