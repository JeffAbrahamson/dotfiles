#!/usr/bin/env python3
"""Announce the nearest Phoenix meal-time window for SSA."""

from datetime import datetime
from zoneinfo import ZoneInfo


def main():
    now = datetime.now(ZoneInfo("America/Phoenix"))
    minute = now.hour * 60 + now.minute
    windows = [
        (7 * 60, 9 * 60, "breakfast"),
        (11 * 60, 13 * 60, "lunch"),
        (17 * 60, 18 * 60, "dinner"),
    ]
    clock = now.strftime("%H:%M")

    for start, end, name in windows:
        if start <= minute < end:
            print(f"{clock} - {name}")
            return

    boundaries = []
    for start, end, name in windows:
        boundaries.append((start, name, "to"))
        boundaries.append((end, name, "since"))

    candidates = []
    for boundary, name, direction in boundaries:
        delta = boundary - minute
        if direction == "since" and delta > 0:
            delta -= 24 * 60
        elif direction == "to" and delta < 0:
            delta += 24 * 60
        candidates.append((abs(delta), delta, name, direction))

    _, delta, name, direction = min(candidates)
    hours, minutes = divmod(abs(delta), 60)
    duration = []
    if hours:
        duration.append(f"{hours} hour" + ("s" if hours != 1 else ""))
    if minutes or not duration:
        duration.append(f"{minutes} minute" + ("s" if minutes != 1 else ""))
    print(f"{clock} - {' '.join(duration)} {direction} {name}")


if __name__ == "__main__":
    main()
