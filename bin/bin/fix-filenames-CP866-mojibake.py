#!/usr/bin/env python3
"""
Fix filenames that have CP866 mojibake and replace spaces with hyphens.

Background: Windows ZIP tools sometimes export filenames whose UTF-8 bytes
get re-interpreted as CP866 (Russian DOS OEM encoding) and then re-encoded
as UTF-8. This produces garbled Cyrillic/box-drawing characters in place of
Western European accented letters (e.g. é -> ├й, è -> ├и, à -> ├а).

Fix: encode the broken UTF-8 string back as CP866, then decode as UTF-8.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path


def fix_name(name: str) -> str:
    try:
        fixed = name.encode("cp866").decode("utf-8")
    except (UnicodeEncodeError, UnicodeDecodeError):
        fixed = name
    return fixed.replace(" ", "-")


def is_hidden_path(path: Path, root: Path) -> bool:
    return any(part.startswith(".") for part in path.relative_to(root).parts)


def rename_tree(root: str, dry_run: bool = False) -> None:
    root_path = Path(root).resolve()
    rename_count = 0

    for dirpath, dirs, files in os.walk(root_path, topdown=True):
        current_dir = Path(dirpath)
        dirs[:] = [
            name
            for name in dirs
            if not is_hidden_path((current_dir / name), root_path)
        ]

        seen_new: set[Path] = set()

        for name in files:
            old_path = current_dir / name
            if is_hidden_path(old_path, root_path):
                continue

            fixed = fix_name(name)
            if name == fixed:
                continue

            new_path = current_dir / fixed
            if new_path.exists() or new_path in seen_new:
                raise FileExistsError(
                    "refusing to rename "
                    f"{old_path} to existing path {new_path}"
                )

            prefix = "[dry-run] " if dry_run else ""
            print(f"{prefix}{name!r} -> {fixed!r}")
            if not dry_run:
                old_path.rename(new_path)
            seen_new.add(new_path)
            rename_count += 1

        for index, name in enumerate(dirs):
            old_path = current_dir / name
            fixed = fix_name(name)
            if name == fixed:
                continue

            new_path = current_dir / fixed
            if new_path.exists() or new_path in seen_new:
                raise FileExistsError(
                    "refusing to rename "
                    f"{old_path} to existing path {new_path}"
                )

            prefix = "[dry-run] " if dry_run else ""
            print(f"{prefix}{name!r} -> {fixed!r}")
            if not dry_run:
                old_path.rename(new_path)
                dirs[index] = fixed
            seen_new.add(new_path)
            rename_count += 1

    action = "Would rename" if dry_run else "Renamed"
    print(f"\n{action}: {rename_count} entries")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "path",
        nargs="?",
        default=".",
        help="directory to process (default: current dir)",
    )
    parser.add_argument(
        "--dry-run",
        "-n",
        action="store_true",
        help="preview changes without renaming",
    )
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    rename_tree(os.path.abspath(args.path), dry_run=args.dry_run)
