import os
import shutil
import subprocess
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
CN2PDF = PROJECT_ROOT / "bin" / "bin" / "cn2pdf"
CN2HTML = PROJECT_ROOT / "bin" / "bin" / "cn2html"

SAMPLE = """\
version: 1
title: {hanzi: 家庭成员, pinyin: jiātíng chéngyuán, english: Family}
source: a test
labels:
  T: {hanzi: 老师, pinyin: lǎoshī, english: Teacher}
sections:
  - items:
      - label: T
        time: "0:04"
        hanzi: 妈妈的爸爸叫外公。
        pinyin: Māma de bàba jiào wàigōng.
        english: Mum's dad is *wàigōng*… & 100% $sure_
        note: a note
      - heading: {hanzi: 第二天, english: The next day}
  - title: {english: Vocabulary}
    items:
      - {hanzi: 外婆, pinyin: wàipó, english: Mother's mother}
"""


def run(command, *args, stdin=None):
    return subprocess.run(
        [str(command), *args],
        input=stdin,
        capture_output=True,
        text=True,
        env={
            **os.environ,
            "PDF_VIEWER": "false",
            "BROWSER": "false",
            "PDF_CLEANUP_DELAY": "0",
        },
    )


@pytest.fixture
def sample(tmp_path: Path) -> Path:
    path = tmp_path / "sample.cn.yaml"
    path.write_text(SAMPLE, encoding="utf-8")
    return path


@pytest.mark.parametrize("command", ["xelatex", "pdftotext"])
def test_cn2pdf_dependency_is_available(command: str) -> None:
    assert shutil.which(command), f"cn2pdf test requires {command}"


def test_cn2pdf_renders_hanzi_pinyin_english_and_labels(
    sample: Path, tmp_path: Path
) -> None:
    pdf = tmp_path / "out.pdf"
    result = run(CN2PDF, "-D", "-o", str(pdf), str(sample))
    assert result.returncode == 0, result.stderr
    assert "Missing character" not in result.stderr
    text = subprocess.run(
        ["pdftotext", str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    compact = "".join(text.split())
    for expected in (
        "家庭成员",
        "老师：妈妈的爸爸叫外公。",
        "lǎoshī:Māmadebàbajiàowàigōng.",
        "Teacher:Mum’sdadiswàigōng…&100%$sure_",
        "第二天",
        "Thenextday",
        "Vocabulary",
        "外婆",
        "0:04",
    ):
        assert expected in compact


def test_cn2html_escapes_and_marks_up(sample: Path) -> None:
    result = run(CN2HTML, "-S", str(sample))
    assert result.returncode == 0, result.stderr
    page = result.stdout
    assert page.startswith("<!doctype html>")
    assert "<strong>老师</strong>：妈妈的爸爸叫外公。" in page
    assert "<em>wàigōng</em>… &amp; 100% $sure_" in page
    assert 'colspan="3"' in page


def test_default_mode_writes_nothing_beside_the_input(
    sample: Path,
) -> None:
    result = run(CN2HTML, str(sample))
    assert result.returncode != 0  # the "false" viewer fails
    assert sorted(p.name for p in sample.parent.iterdir()) == [
        "sample.cn.yaml"
    ]


def test_write_option_names_output_after_input(sample: Path) -> None:
    result = run(CN2HTML, "-w", "-D", str(sample))
    assert result.returncode == 0, result.stderr
    assert (sample.parent / "sample.html").is_file()
    again = run(CN2HTML, "-w", "-D", str(sample))
    assert "Using existing" in again.stdout


def test_reads_stdin(sample: Path) -> None:
    result = run(CN2HTML, "-S", stdin=sample.read_text(encoding="utf-8"))
    assert result.returncode == 0, result.stderr
    assert "家庭成员" in result.stdout


@pytest.mark.parametrize(
    ("document", "message"),
    [
        ("items:\n  - {hanzi: 好, englsh: good}\n", "unknown keys"),
        ("items:\n  - {pinyin: hǎo}\n", "needs hanzi or english"),
        (
            "labels: {T: Teacher}\nitems:\n  - {label: X, hanzi: 好}\n",
            "unknown label 'X'",
        ),
        ("version: 2\nitems: []\n", "unsupported version"),
        ("items: [\n", "invalid YAML"),
    ],
)
def test_invalid_documents_are_reported(document: str, message: str) -> None:
    result = run(CN2PDF, "-S", stdin=document)
    assert result.returncode != 0
    assert message in result.stderr
