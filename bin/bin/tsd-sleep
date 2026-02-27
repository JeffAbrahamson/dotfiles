#!/usr/bin/env python3
"""tsd-sleep.py — Sleep hygiene visualiser.

Reads $TSD_DIR/{sleep-quality,sleep-start,sleep-end} and plots a summary
of sleep patterns: bedtime regularity, wake-time distribution, sleep
duration, and quality trends.
"""

import os
import sys
from pathlib import Path

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

# ── Config ───────────────────────────────────────────────────────────────────

_tsd_candidates = [
    os.environ.get("TSD_DIR"),
    Path.home() / "tsd",
    Path.home() / "common" / "data" / "tsd",
    Path.home() / "data" / "tsd",
]
TSD_DIR = next(
    Path(p) for p in _tsd_candidates
    if p and (Path(p) / "sleep-quality").exists()
)
RECENT_DAYS = 90
ROLL_DAYS = 28  # rolling-average window

QUALITY_LABELS = {1: "bad", 2: "meh", 3: "good", 4: "awesome"}

# Plausible HHMM ranges for filtering obvious data-entry errors.
START_MAX_HHMM = 2700   # up to ~03:00 next day
END_MAX_HHMM   = 1400   # wake no later than 14:00

# ── Helpers ───────────────────────────────────────────────────────────────────

def load_tsd(name: str) -> pd.DataFrame:
    path = TSD_DIR / name
    df = pd.read_csv(
        path, sep=r"\s+", header=None, names=["date", "value"],
        parse_dates=["date"],
    )
    return df


def hhmm_to_min(s: pd.Series) -> pd.Series:
    """Convert HHMM int/float series (e.g. 2236) to minutes from midnight."""
    v = s.astype(int)
    return (v // 100) * 60 + (v % 100)


def min_to_hhmm_str(m) -> str:
    m = int(m) % 1440
    return f"{m // 60:02d}:{m % 60:02d}"


def bedtime_adj(minutes: pd.Series) -> pd.Series:
    """
    Shift bedtimes < noon forward by 24 h so that 00:xx lands just above
    24:xx on a continuous axis rather than near 00:xx.
    """
    return minutes.where(minutes >= 18 * 60, minutes + 1440)


def fmt_hour_tick(x, _):
    """Matplotlib tick formatter: fractional hours → HH:MM."""
    total_min = int(round(x * 60)) % 1440
    return f"{total_min // 60:02d}:{total_min % 60:02d}"


# ── Load & clean ──────────────────────────────────────────────────────────────

quality_raw = load_tsd("sleep-quality").rename(columns={"value": "quality"})
start_raw   = load_tsd("sleep-start").rename(columns={"value": "start_hhmm"})
end_raw     = load_tsd("sleep-end").rename(columns={"value": "end_hhmm"})

# Filter implausible values (data-entry errors).
start_raw = start_raw[start_raw["start_hhmm"] < START_MAX_HHMM]
end_raw   = end_raw[end_raw["end_hhmm"] < END_MAX_HHMM]

# Quality: round 2.5 → 3, clip to [1,4].
quality_raw["quality"] = (
    quality_raw["quality"].round().clip(1, 4).astype(int)
)

# Merge on date (outer so we keep every day regardless of which files have it).
df = (
    start_raw
    .merge(end_raw, on="date", how="outer")
    .merge(quality_raw, on="date", how="outer")
    .sort_values("date")
    .reset_index(drop=True)
)

# Derived columns (computed only where source data is present).
mask_start = df["start_hhmm"].notna()
mask_end   = df["end_hhmm"].notna()

df["start_min"] = np.nan
df["end_min"]   = np.nan

df.loc[mask_start, "start_min"] = hhmm_to_min(df.loc[mask_start, "start_hhmm"])
df.loc[mask_end,   "end_min"]   = hhmm_to_min(df.loc[mask_end,   "end_hhmm"])

# Duration: (end - start) mod 1440 — handles crossing midnight correctly.
both = mask_start & mask_end
df["duration_min"] = np.nan
df.loc[both, "duration_min"] = (
    df.loc[both, "end_min"] - df.loc[both, "start_min"]
) % 1440

# Drop obviously wrong durations (< 2 h or > 13 h).
bad_dur = df["duration_min"].notna() & ~df["duration_min"].between(120, 780)
df.loc[bad_dur, "duration_min"] = np.nan

# Adjusted bedtime for plotting (hours, continuous axis past midnight).
df["start_hr_adj"] = bedtime_adj(df["start_min"]) / 60

df["date"] = pd.to_datetime(df["date"])
cutoff  = df["date"].max() - pd.Timedelta(days=RECENT_DAYS)
recent  = df[df["date"] >= cutoff]

# Day-of-week.
df["dow"] = df["date"].dt.day_name()

# ── Text summary ──────────────────────────────────────────────────────────────

sep = "=" * 62
print(sep)
print(f"  SLEEP SUMMARY   all-time vs last {RECENT_DAYS} days")
print(sep)
print(f"  Date range : {df['date'].min().date()} → {df['date'].max().date()}")
print(f"  Records    : quality={quality_raw.shape[0]:,}  "
      f"start={start_raw.shape[0]:,}  end={end_raw.shape[0]:,}")
print()

def row(label, all_s, rec_s):
    a = all_s.dropna()
    r = rec_s.dropna()
    a_mean = min_to_hhmm_str(a.mean()) if not a.empty else "—"
    r_mean = min_to_hhmm_str(r.mean()) if not r.empty else "—"
    a_std  = f"{a.std():.0f}min"       if not a.empty else "—"
    r_std  = f"{r.std():.0f}min"       if not r.empty else "—"
    print(f"  {label:<18} all-time  mean={a_mean}  σ={a_std}")
    print(f"  {'':<18} recent    mean={r_mean}  σ={r_std}")
    print()

row("Bedtime",   df["start_hr_adj"] * 60,    recent["start_hr_adj"] * 60)
row("Wake time", df["end_min"],             recent["end_min"])

a_dur = df["duration_min"].dropna() / 60
r_dur = recent["duration_min"].dropna() / 60
print(f"  {'Duration':<18} all-time  mean={a_dur.mean():.1f}h  σ={a_dur.std()*60:.0f}min")
print(f"  {'':<18} recent    mean={r_dur.mean():.1f}h  σ={r_dur.std()*60:.0f}min")
print()

print("  Quality:")
q_all = quality_raw["quality"].value_counts().sort_index()
q_rec = recent["quality"].dropna().astype(int).value_counts().sort_index()
print(f"  {'Band':<14} {'All-n':>7} {'All-%':>7} {'Rec-n':>7} {'Rec-%':>7}")
for q in range(1, 5):
    a = q_all.get(q, 0)
    r = q_rec.get(q, 0)
    ap = 100 * a / q_all.sum()
    rp = 100 * r / q_rec.sum() if q_rec.sum() > 0 else 0.0
    label = f"{q} {QUALITY_LABELS[q]}"
    print(f"  {label:<14} {a:>7}  {ap:>5.1f}%  {r:>7}  {rp:>5.1f}%")

a_qmean = quality_raw["quality"].mean()
r_qmean = recent["quality"].dropna().mean()
print(f"  {'mean':<14} {a_qmean:>13.2f}  {r_qmean:>13.2f}")
print()

# Bedtime regularity: % of nights within ±30 min of personal median.
bt     = df["start_min"].dropna()
bt_rec = recent["start_min"].dropna()
tol    = 30
reg_all = 100 * ((bt - bt.median()).abs() <= tol).mean()
reg_rec = 100 * ((bt_rec - bt_rec.median()).abs() <= tol).mean()
print(f"  Bedtime regularity (within ±{tol} min of median)")
print(f"    all-time: {reg_all:.0f}%   recent: {reg_rec:.0f}%")
print()

# Day-of-week bedtime summary.
print("  Median bedtime by day of week (all time):")
dow_order_full = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
dow_bt = (
    df.dropna(subset=["start_min"])
    .groupby("dow")["start_min"]
    .median()
    .reindex(dow_order_full)
)
for day, m in dow_bt.items():
    if not np.isnan(m):
        print(f"    {day:<12} {min_to_hhmm_str(m)}")
print()

# ── Figure 1: Distributions ───────────────────────────────────────────────────

sns.set_theme(style="darkgrid", palette="muted")

fig1, axes = plt.subplots(2, 2, figsize=(14, 10))
fig1.suptitle(
    f"Sleep distributions — grey=all time, blue=last {RECENT_DAYS} days",
    fontsize=13,
)

# 1a: Bedtime histogram
ax = axes[0, 0]
all_bt = df["start_hr_adj"].dropna()
rec_bt = bedtime_adj(recent["start_min"].dropna()) / 60
ax.hist(all_bt, bins=48, color="grey", alpha=0.5, label="All time")
ax.hist(rec_bt, bins=48, color="steelblue", alpha=0.7, label=f"Last {RECENT_DAYS}d")
ax.xaxis.set_major_formatter(plt.FuncFormatter(fmt_hour_tick))
ax.xaxis.set_major_locator(plt.MultipleLocator(1.0))
plt.setp(ax.xaxis.get_majorticklabels(), rotation=45, ha="right")
ax.axvline(all_bt.mean(), color="grey",      ls="--", lw=1.5,
           label=f"Mean {fmt_hour_tick(all_bt.mean(), None)}")
ax.axvline(rec_bt.mean(), color="steelblue", ls="--", lw=1.5,
           label=f"Recent {fmt_hour_tick(rec_bt.mean(), None)}")
ax.set_xlabel("Bedtime")
ax.set_ylabel("Nights")
ax.set_title("Bedtime distribution")
ax.legend(fontsize=8)

# 1b: Wake-time histogram
ax = axes[0, 1]
all_wt = df["end_min"].dropna() / 60
rec_wt = recent["end_min"].dropna() / 60
ax.hist(all_wt, bins=48, color="grey",      alpha=0.5, label="All time")
ax.hist(rec_wt, bins=48, color="steelblue", alpha=0.7, label=f"Last {RECENT_DAYS}d")
ax.xaxis.set_major_formatter(plt.FuncFormatter(fmt_hour_tick))
ax.xaxis.set_major_locator(plt.MultipleLocator(1.0))
plt.setp(ax.xaxis.get_majorticklabels(), rotation=45, ha="right")
ax.axvline(all_wt.mean(), color="grey",      ls="--", lw=1.5,
           label=f"Mean {fmt_hour_tick(all_wt.mean(), None)}")
ax.axvline(rec_wt.mean(), color="steelblue", ls="--", lw=1.5,
           label=f"Recent {fmt_hour_tick(rec_wt.mean(), None)}")
ax.set_xlabel("Wake time")
ax.set_title("Wake-time distribution")
ax.legend(fontsize=8)

# 1c: Sleep duration
ax = axes[1, 0]
all_dur_h = df["duration_min"].dropna() / 60
rec_dur_h = recent["duration_min"].dropna() / 60
ax.hist(all_dur_h, bins=40, color="grey",      alpha=0.5, label="All time")
ax.hist(rec_dur_h, bins=40, color="steelblue", alpha=0.7, label=f"Last {RECENT_DAYS}d")
ax.axvline(all_dur_h.mean(), color="grey",      ls="--", lw=1.5,
           label=f"Mean {all_dur_h.mean():.1f}h")
ax.axvline(rec_dur_h.mean(), color="steelblue", ls="--", lw=1.5,
           label=f"Recent {rec_dur_h.mean():.1f}h")
ax.axvline(7, color="red", ls=":", lw=1, alpha=0.7, label="7h target")
ax.set_xlabel("Hours")
ax.set_title("Sleep duration")
ax.legend(fontsize=8)

# 1d: Quality distribution — grouped bar
ax = axes[1, 1]
q_bands   = [1, 2, 3, 4]
q_all_pct = [q_all.get(q, 0) / q_all.sum() * 100 for q in q_bands]
q_rec_pct = [
    q_rec.get(q, 0) / q_rec.sum() * 100 if q_rec.sum() > 0 else 0.0
    for q in q_bands
]
x = np.arange(4)
w = 0.38
ax.bar(x - w/2, q_all_pct, w, color="grey",      alpha=0.7, label="All time")
ax.bar(x + w/2, q_rec_pct, w, color="steelblue", alpha=0.7, label=f"Last {RECENT_DAYS}d")
ax.set_xticks(x)
ax.set_xticklabels([f"{q}\n{QUALITY_LABELS[q]}" for q in q_bands])
ax.set_ylabel("%")
ax.set_title("Quality distribution")
ax.legend(fontsize=8)

fig1.tight_layout()

# ── Figure 2: Time series ─────────────────────────────────────────────────────

fig2, axes2 = plt.subplots(3, 1, figsize=(15, 11), sharex=True)
fig2.suptitle(
    f"Sleep over time  (dots=nightly, line={ROLL_DAYS}-day rolling mean)",
    fontsize=13,
)

# 2a: Bedtime
ax = axes2[0]
plot_bt = df[["date", "start_hr_adj"]].dropna()
ax.scatter(plot_bt["date"], plot_bt["start_hr_adj"], s=3, alpha=0.25, color="steelblue")
roll_bt = (
    plot_bt.set_index("date")["start_hr_adj"]
    .rolling(f"{ROLL_DAYS}D", min_periods=7)
    .mean()
)
ax.plot(roll_bt.index, roll_bt.values, color="darkblue", lw=1.8)
ax.yaxis.set_major_formatter(plt.FuncFormatter(fmt_hour_tick))
ax.yaxis.set_major_locator(plt.MultipleLocator(1.0))
ax.xaxis.set_major_locator(mdates.YearLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
ax.set_ylabel("Bedtime")
ax.set_title("Bedtime")

# 2b: Sleep duration
ax = axes2[1]
plot_dur = df[["date", "duration_min"]].dropna().copy()
plot_dur["dur_h"] = plot_dur["duration_min"] / 60
ax.scatter(plot_dur["date"], plot_dur["dur_h"], s=3, alpha=0.25, color="seagreen")
roll_dur = (
    plot_dur.set_index("date")["dur_h"]
    .rolling(f"{ROLL_DAYS}D", min_periods=7)
    .mean()
)
ax.plot(roll_dur.index, roll_dur.values, color="darkgreen", lw=1.8)
ax.axhline(7, color="red", ls=":", lw=1.2, alpha=0.7, label="7h")
ax.xaxis.set_major_locator(mdates.YearLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
ax.set_ylabel("Hours")
ax.set_title("Sleep duration")
ax.legend(fontsize=8, loc="upper right")

# 2c: Quality
ax = axes2[2]
plot_q = df[["date", "quality"]].dropna().copy()
plot_q["quality"] = plot_q["quality"].astype(float)
ax.scatter(plot_q["date"], plot_q["quality"], s=3, alpha=0.25, color="darkorange",
           zorder=2)
roll_q = (
    plot_q.set_index("date")["quality"]
    .rolling(f"{ROLL_DAYS}D", min_periods=7)
    .mean()
)
ax.plot(roll_q.index, roll_q.values, color="saddlebrown", lw=1.8)
ax.set_yticks([1, 2, 3, 4])
ax.set_yticklabels([f"{q} ({QUALITY_LABELS[q]})" for q in [1, 2, 3, 4]])
ax.xaxis.set_major_locator(mdates.YearLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
ax.set_ylabel("Quality")
ax.set_title("Sleep quality")

# sharex=True aligns the axes; force year tick labels visible on all panels
# (sharex hides them on non-bottom panels by default).
for ax in axes2:
    plt.setp(ax.xaxis.get_majorticklabels(), visible=True)

fig2.tight_layout()

# ── Figure 3: Day-of-week patterns ───────────────────────────────────────────

fig3, axes3 = plt.subplots(1, 2, figsize=(13, 5))
fig3.suptitle("Day-of-week patterns", fontsize=13)

dow_order = ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]
df["dow_short"] = pd.Categorical(
    df["date"].dt.strftime("%a"), categories=dow_order, ordered=True
)

# 3a: Bedtime by day-of-week
ax = axes3[0]
sns.boxplot(
    data=df.dropna(subset=["start_hr_adj"]),
    x="dow_short", y="start_hr_adj",
    order=dow_order, ax=ax,
    color="steelblue", linewidth=0.8, fliersize=1.5,
)
ax.yaxis.set_major_formatter(plt.FuncFormatter(fmt_hour_tick))
ax.set_xlabel("")
ax.set_ylabel("Bedtime")
ax.set_title("Bedtime by day of week")

# 3b: Quality by day-of-week
ax = axes3[1]
df["quality_f"] = df["quality"].astype(float)
sns.boxplot(
    data=df.dropna(subset=["quality_f"]),
    x="dow_short", y="quality_f",
    order=dow_order, ax=ax,
    color="darkorange", linewidth=0.8, fliersize=1.5,
)
ax.set_yticks([1, 2, 3, 4])
ax.set_yticklabels([f"{q} ({QUALITY_LABELS[q]})" for q in [1, 2, 3, 4]])
ax.set_xlabel("")
ax.set_ylabel("Quality")
ax.set_title("Sleep quality by day of week")

fig3.tight_layout()

plt.show()
