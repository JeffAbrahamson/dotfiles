# Bin Programs

This directory contains the actual executables installed from the repository. Most are small personal utilities, but a few are substantial enough to deserve a quick pointer here.

## Utility groups

* Network and connectivity: [`bandwidth`](bandwidth), [`bandwidth_history.py`](bandwidth_history.py), [`speedtest`](speedtest), [`up-monitor`](up-monitor), [`wifi-qr`](wifi-qr), and [`wifi_summary.py`](wifi_summary.py).
* Time-series and personal data: [`tsd-bicycle`](tsd-bicycle) and [`tsd-sleep`](tsd-sleep); general tsd plotting commands (`tsd-plot`, `tsd-season-plot`) are installed with the `tsd` package.
* File and package comparison: [`compare-file-lists`](compare-file-lists), [`compare-sum-lists`](compare-sum-lists), [`dedup`](dedup), and [`disk-usage.sh`](disk-usage.sh).
* Git helpers: [`gc`](gc), [`git-this-week`](git-this-week),
  [`git-remote-changes`](git-remote-changes),
  [`git-update-all.sh`](git-update-all.sh), and [`update-gf`](update-gf).
* Window-manager and desktop helpers: [`i3-chromium-browser`](i3-chromium-browser), [`signal-desktop`](signal-desktop), [`journal-edit`](journal-edit), and [`journal-read`](journal-read).
* Media: [`video-to-audio`](video-to-audio) copies a video's first audio
  stream without re-encoding it and can optionally cut it by start and end
  time. [`ls-audio`](ls-audio) lists files with the duration of recognized
  audio and video files, caching probe results per directory.

## Notable tools

* [`bandwidth`](bandwidth) is the cleaner current interface for summarizing recorded network quality data, including per-network breakdowns and textual or graphical statistics.
* [`md2pdf`](md2pdf) converts Markdown to an A4 PDF with Pandoc and XeLaTeX.
  Fenced `mermaid` blocks are rendered as fitted vector diagrams when Mermaid
  CLI is installed with `npm install -g @mermaid-js/mermaid-cli`; otherwise,
  they remain ordinary code blocks and a warning is printed.
* [`tsd-bicycle`](tsd-bicycle) focuses on bicycle mileage logs and can report totals or generate year-over-year plots.

`git-remote-changes [DIRECTORY]` recursively fetches Git worktrees at or below
the directory (the current directory by default) and lists repositories whose
remote-tracking branches changed. It does not fetch or prune tags, pull, or
alter a worktree.
Because the previous remote-tracking refs are its baseline, running it
acknowledges the changes it reports.

`git-update-all.sh [--prune-local]` fetches and prunes remote branches for each
repository directly below the current directory, then safely fast-forwards
the current branch when its worktree is pristine and shows its status.
Fetching is confined to conventional remote-tracking refs; local branches and
tags are never fetch-prune targets.
By default it only reports local branches whose upstreams are gone. With
`--prune-local`, matching local branches are removed; clean linked worktrees
are removed with them, while the primary worktree switches to the remote's
default branch. Dirty, divergent, locked, or unverifiable branches and
worktrees are preserved.

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
