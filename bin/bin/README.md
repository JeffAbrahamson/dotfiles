# Bin Programs

This directory contains the actual executables installed from the repository. Most are small personal utilities, but a few are substantial enough to deserve a quick pointer here.

## Utility groups

* Network and connectivity: [`bandwidth`](bandwidth), [`bandwidth_history.py`](bandwidth_history.py), [`speedtest`](speedtest), [`up-monitor`](up-monitor), [`wifi-qr`](wifi-qr), and [`wifi_summary.py`](wifi_summary.py).
* Time-series and personal data: [`tsd-bicycle`](tsd-bicycle) and [`tsd-sleep`](tsd-sleep); general tsd plotting commands (`tsd-plot`, `tsd-season-plot`) are installed with the `tsd` package.
* File and package comparison: [`compare-file-lists`](compare-file-lists), [`compare-sum-lists`](compare-sum-lists), [`dedup`](dedup), and [`disk-usage.sh`](disk-usage.sh).
* Git helpers: [`gc`](gc), [`git-this-week`](git-this-week), [`git-update-all.sh`](git-update-all.sh), and [`update-gf`](update-gf).
* Window-manager and desktop helpers: [`i3-chromium-browser`](i3-chromium-browser), [`signal-desktop`](signal-desktop), [`journal-edit`](journal-edit), and [`journal-read`](journal-read).
* Media: [`video-to-audio`](video-to-audio) copies a video's first audio
  stream without re-encoding it and can optionally cut it by start and end
  time. [`ls-audio`](ls-audio) lists files with the duration of recognized
  audio and video files, caching probe results per directory.

## Notable tools

* [`bandwidth`](bandwidth) is the cleaner current interface for summarizing recorded network quality data, including per-network breakdowns and textual or graphical statistics.
* [`tsd-bicycle`](tsd-bicycle) focuses on bicycle mileage logs and can report totals or generate year-over-year plots.

`video-to-audio VIDEO [START [END]]` accepts times as seconds, `MM:SS`, or
`HH:MM:SS`. It requires `ffmpeg` and `ffprobe`. The output extension follows
the source audio codec (for example, Opus becomes `.opus`), and unknown codecs
use the Matroska audio `.mka` container. The result's packet timestamps are
checked automatically. On failure, the command retries with accurate
output-side seeking and then with a Matroska `.mka` container. Use
`--accurate-seek` or `--matroska` to select either fallback immediately.

`ls-audio [OPTION]... [FILE]...` supports the familiar `-a`, `-l`, `-h`, and
`-d` listing options. `-s` adds allocated sizes in 1K blocks and combines with
`-h` as `-sh`. Output is colored when it goes to a terminal; use
`--color=always`, `--color=auto`, or `--color=never` to override this. It uses
`ffprobe` for recognized media filename extensions and stores results in
`.ls-audio-cache`; `--no-cache` disables this. Cache entries include inode,
device, size, modification time, and change time, so replaced or edited files
are probed again. Use
`--duration-ascending` (also `--sort-duration` and `--asc`) to list shortest
media first, or `--duration-descending` (also `--sort-duration-descending` and
`--desc`) to list longest media first. Files without a known duration appear
after media files.
