from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import subprocess
import sys
from pathlib import Path
from types import ModuleType

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = PROJECT_ROOT / "bin" / "bin" / "ls-audio"


def load_script() -> ModuleType:
    loader = importlib.machinery.SourceFileLoader("ls_audio", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


ls_audio = load_script()


def completed(duration: str) -> subprocess.CompletedProcess[str]:
    return subprocess.CompletedProcess([], 0, duration, "")


def test_duration_is_probed_then_read_from_cache(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    recording = tmp_path / "recording.flac"
    recording.write_bytes(b"audio")
    calls: list[list[str]] = []

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        calls.append(command)
        return completed("125.4\n")

    monkeypatch.setattr(ls_audio.subprocess, "run", fake_run)

    ls_audio.list_directory(tmp_path, False, False, False, True)
    ls_audio.list_directory(tmp_path, False, False, False, True)

    assert len(calls) == 1
    assert calls[0][-1] == str(recording)
    cache = json.loads((tmp_path / ls_audio.CACHE_NAME).read_text())
    assert list(cache["files"]) == ["recording.flac"]
    assert cache["files"]["recording.flac"]["duration"] == 125.4


def test_changed_file_is_probed_again(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    recording = tmp_path / "recording.mp3"
    recording.write_bytes(b"first")
    calls = 0

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        nonlocal calls
        calls += 1
        return completed("10")

    monkeypatch.setattr(ls_audio.subprocess, "run", fake_run)
    ls_audio.list_directory(tmp_path, False, False, False, True)
    recording.write_bytes(b"second, and now longer")
    ls_audio.list_directory(tmp_path, False, False, False, True)

    assert calls == 2


def test_non_media_files_are_not_opened_by_ffprobe(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    (tmp_path / "notes.txt").write_text("hello")

    def unexpected_run(*args: object, **kwargs: object) -> None:
        pytest.fail("ffprobe was called for a non-media file")

    monkeypatch.setattr(ls_audio.subprocess, "run", unexpected_run)
    ls_audio.list_directory(tmp_path, False, False, False, True)

    assert not (tmp_path / ls_audio.CACHE_NAME).exists()


def test_listing_formats_duration_and_hides_cache(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    (tmp_path / "clip.mkv").touch()
    monkeypatch.setattr(
        ls_audio.subprocess, "run", lambda *args, **kwargs: completed("3661")
    )

    assert ls_audio.main([str(tmp_path)]) == 0

    output = capsys.readouterr().out
    assert "1:01:01  clip.mkv" in output
    assert ls_audio.CACHE_NAME not in output


def test_bad_media_file_is_negatively_cached(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    (tmp_path / "broken.ogg").touch()
    calls = 0

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        nonlocal calls
        calls += 1
        return subprocess.CompletedProcess(command, 1, "", "bad file")

    monkeypatch.setattr(ls_audio.subprocess, "run", fake_run)
    ls_audio.list_directory(tmp_path, False, False, False, True)
    ls_audio.list_directory(tmp_path, False, False, False, True)

    assert calls == 1


@pytest.mark.parametrize(
    ("option", "expected"),
    [
        ("--duration-ascending", ["short.opus", "long.opus"]),
        ("--duration-descending", ["long.opus", "short.opus"]),
        ("--asc", ["short.opus", "long.opus"]),
        ("--desc", ["long.opus", "short.opus"]),
    ],
)
def test_sorts_by_duration(
    option: str,
    expected: list[str],
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    capsys: pytest.CaptureFixture[str],
) -> None:
    (tmp_path / "long.opus").touch()
    (tmp_path / "short.opus").touch()

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        duration = "120" if command[-1].endswith("long.opus") else "30"
        return completed(duration)

    monkeypatch.setattr(ls_audio.subprocess, "run", fake_run)

    assert ls_audio.main([option, "--no-cache", str(tmp_path)]) == 0

    output_lines = capsys.readouterr().out.splitlines()
    names = [line.split(maxsplit=1)[1] for line in output_lines]
    assert names == expected


def test_quotes_only_shell_sensitive_names() -> None:
    accented = "piano-à-4-mains.opus"
    assert ls_audio.quote_name(accented) == accented
    assert ls_audio.quote_name("two words.opus") == "'two words.opus'"
    assert ls_audio.quote_name("music-🎵.opus") == "music-🎵.opus"


def test_combined_size_and_human_readable_options() -> None:
    args = ls_audio.parse_args(["-sh"])

    assert args.size
    assert args.human_readable


def test_size_option_prints_allocated_blocks(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    source = tmp_path / "notes.txt"
    source.write_text("hello")

    assert ls_audio.main(["-s", "--color=never", str(source)]) == 0

    fields = capsys.readouterr().out.split()
    expected_blocks = str((source.stat().st_blocks + 1) // 2)
    assert fields[:2] == [expected_blocks, "-"]


def test_color_always_colors_duration_size_and_directory(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    child = tmp_path / "child"
    child.mkdir()

    assert ls_audio.main(["-s", "--color=always", str(tmp_path)]) == 0

    output = capsys.readouterr().out
    assert ls_audio.COLOR_DURATION in output
    assert ls_audio.COLOR_SIZE in output
    assert ls_audio.COLOR_DIRECTORY in output
    assert output.count(ls_audio.COLOR_RESET) >= 3


def test_size_listing_of_directory_prints_total(
    tmp_path: Path, capsys: pytest.CaptureFixture[str]
) -> None:
    (tmp_path / "notes.txt").write_text("hello")

    assert ls_audio.main(["-s", "--color=never", str(tmp_path)]) == 0

    assert capsys.readouterr().out.startswith("total ")
