from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SCRIPT_PATH = PROJECT_ROOT / "bin" / "bin" / "fix-filenames-CP866-mojibake.py"

SPEC = importlib.util.spec_from_file_location(
    "fix_filenames_cp866",
    SCRIPT_PATH,
)
assert SPEC is not None
assert SPEC.loader is not None
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)

fix_name = MODULE.fix_name
rename_tree = MODULE.rename_tree


def mojibake_name(text: str) -> str:
    return text.encode("utf-8").decode("cp866")


def test_fix_name_recovers_cp866_mojibake_and_replaces_spaces() -> None:
    broken = mojibake_name("café au lait.txt")

    assert fix_name(broken) == "café-au-lait.txt"


def test_fix_name_replaces_spaces_without_mojibake() -> None:
    assert fix_name("plain name.txt") == "plain-name.txt"


def test_rename_tree_skips_hidden_directories(tmp_path: Path) -> None:
    hidden_dir = tmp_path / ".git"
    hidden_dir.mkdir()
    hidden_file = hidden_dir / mojibake_name("café.txt")
    hidden_file.write_text("hidden", encoding="utf-8")

    visible_file = tmp_path / mojibake_name("résumé.txt")
    visible_file.write_text("visible", encoding="utf-8")

    rename_tree(str(tmp_path))

    assert hidden_file.exists()
    assert not (hidden_dir / "café.txt").exists()
    assert not visible_file.exists()
    assert (tmp_path / "résumé.txt").exists()


def test_rename_tree_descends_into_renamed_directories(tmp_path: Path) -> None:
    broken_dir = tmp_path / mojibake_name("café")
    broken_dir.mkdir()
    broken_file = broken_dir / mojibake_name("résumé.txt")
    broken_file.write_text("nested", encoding="utf-8")

    rename_tree(str(tmp_path))

    assert not broken_dir.exists()
    assert not broken_file.exists()
    assert (tmp_path / "café").is_dir()
    assert (tmp_path / "café" / "résumé.txt").exists()


def test_rename_tree_rejects_collisions(tmp_path: Path) -> None:
    broken_file = tmp_path / mojibake_name("café.txt")
    broken_file.write_text("broken", encoding="utf-8")
    colliding_file = tmp_path / "café.txt"
    colliding_file.write_text("existing", encoding="utf-8")

    with pytest.raises(FileExistsError, match="existing path"):
        rename_tree(str(tmp_path))

    assert broken_file.exists()
    assert colliding_file.exists()


def test_rename_tree_dry_run_does_not_modify_files(tmp_path: Path) -> None:
    broken_file = tmp_path / mojibake_name("café.txt")
    broken_file.write_text("broken", encoding="utf-8")

    rename_tree(str(tmp_path), dry_run=True)

    assert broken_file.exists()
    assert not (tmp_path / "café.txt").exists()
