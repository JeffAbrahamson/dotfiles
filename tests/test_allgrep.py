from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = PROJECT_ROOT / "bin" / "bin" / "allgrep"


def allgrep(cwd: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), *args],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )


@pytest.fixture
def work(tmp_path: Path) -> Path:
    (tmp_path / "finance" / "France" / "research").mkdir(parents=True)
    (
        tmp_path / "finance" / "France" / "research" / "inheritance.md"
    ).write_text("US estate tax\nunrelated line\nTax treaty\n")
    (tmp_path / "notes" / "research").mkdir(parents=True)
    (tmp_path / "notes" / "research" / "tea.md").write_text("black tea\n")
    (tmp_path / "notes" / "research" / "tea.pdf").write_bytes(b"%PDF\0tax")
    (tmp_path / "notes" / "research" / "teabag.pdf").write_bytes(b"%PDF\0tax")
    return tmp_path


def test_backups_and_pdfs_generated_from_markdown_are_skipped(
    tmp_path: Path,
) -> None:
    (tmp_path / "tea.md").write_text("black tea\n")
    (tmp_path / "tea.md~").write_text("black tea\n")
    (tmp_path / "tea.md.~1~").write_text("black tea\n")
    (tmp_path / "tea.PDF").write_bytes(b"%PDF\0")
    (tmp_path / "tea-scan.pdf").write_bytes(b"%PDF\0")

    result = allgrep(tmp_path, "-l", "tea")

    assert result.stdout.splitlines() == ["tea-scan.pdf", "tea.md"]


def test_keywords_must_all_occur_in_contents(work: Path) -> None:
    result = allgrep(work / "finance", "estate", "treaty")

    assert result.returncode == 0
    assert result.stdout.splitlines() == [
        "France/research/inheritance.md:1:US estate tax",
        "France/research/inheritance.md:3:Tax treaty",
    ]


def test_keyword_may_match_the_path(work: Path) -> None:
    result = allgrep(work, "France", "estate")

    assert result.stdout.splitlines() == [
        "finance/France/research/inheritance.md:1:US estate tax",
    ]


def test_file_matching_only_by_path_is_listed_by_name(work: Path) -> None:
    result = allgrep(work, "research", "tea")

    assert result.stdout.splitlines() == [
        "notes/research/tea.md:1:black tea",
        "notes/research/teabag.pdf",
    ]


def test_binary_contents_are_not_searched(work: Path) -> None:
    result = allgrep(work / "notes", "tax")

    assert result.returncode == 1
    assert result.stdout == ""


def test_ignore_case(work: Path) -> None:
    result = allgrep(work / "finance", "-i", "TAX")

    assert result.stdout.splitlines() == [
        "France/research/inheritance.md:1:US estate tax",
        "France/research/inheritance.md:3:Tax treaty",
    ]


def test_files_with_matches(work: Path) -> None:
    result = allgrep(work, "-l", "tax")

    assert result.stdout.splitlines() == [
        "finance/France/research/inheritance.md",
    ]


def test_dirs_prefix_paths_and_skip_nested(work: Path) -> None:
    result = allgrep(
        work,
        "--dir",
        "notes/research/",
        "--dir",
        "finance/France/research",
        "--dir",
        "notes",
        "-l",
        "research",
    )

    assert result.stdout.splitlines() == [
        "finance/France/research/inheritance.md",
        "notes/research/tea.md",
        "notes/research/teabag.pdf",
    ]


def test_missing_dir_is_an_error(work: Path) -> None:
    result = allgrep(work, "--dir", "missing", "tax")

    assert result.returncode == 2
    assert "not a directory: missing" in result.stderr


def test_help_has_see_also(work: Path) -> None:
    result = allgrep(work, "--help")

    assert result.returncode == 0
    assert "See also:" in result.stdout
    assert "resgrep" in result.stdout
