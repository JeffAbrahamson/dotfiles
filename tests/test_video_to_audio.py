from __future__ import annotations

import json
import subprocess
import sys
from decimal import Decimal
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[1]
PYTHON_DIR = PROJECT_ROOT / "python"
if str(PYTHON_DIR) not in sys.path:
    sys.path.insert(0, str(PYTHON_DIR))

import video_to_audio  # noqa: E402
from video_to_audio import (  # noqa: E402
    VideoToAudioError,
    build_ffmpeg_command,
    extract_audio,
    output_path_for,
    parse_optional_times,
    parse_time,
)


@pytest.mark.parametrize(
    ("text", "seconds"),
    [
        ("3", Decimal(3)),
        ("33", Decimal(33)),
        ("333", Decimal(333)),
        ("12.5", Decimal("12.5")),
        ("02:03", Decimal(123)),
        ("1:02:03.5", Decimal("3723.5")),
    ],
)
def test_parse_time(text: str, seconds: Decimal) -> None:
    assert parse_time(text) == seconds


@pytest.mark.parametrize(
    "text",
    ["", "abc", "1:60", "1:60:00", "1:2:3:4", "-1", "1::2"],
)
def test_parse_time_rejects_invalid_values(text: str) -> None:
    with pytest.raises(ValueError):
        parse_time(text)


def test_parse_optional_times_reports_value_and_format() -> None:
    with pytest.raises(VideoToAudioError) as error:
        parse_optional_times(["bad"])

    assert "start time 'bad'" in str(error.value)
    assert "seconds, MM:SS" in str(error.value)


def test_parse_optional_times_requires_end_after_start() -> None:
    with pytest.raises(VideoToAudioError, match="must be later"):
        parse_optional_times(["10", "5"])


@pytest.mark.parametrize(
    ("codec", "suffix"),
    [("opus", ".opus"), ("aac", ".m4a"), ("vorbis", ".ogg"), ("x", ".mka")],
)
def test_output_path_for(codec: str, suffix: str) -> None:
    assert output_path_for(Path("movie.mkv"), codec) == Path(f"movie{suffix}")


def test_build_command_copies_audio() -> None:
    command = build_ffmpeg_command(
        Path("movie.mkv"),
        Path("movie.opus"),
        Decimal("62.5"),
        Decimal("120"),
    )
    assert command == [
        "ffmpeg",
        "-v",
        "error",
        "-nostdin",
        "-ss",
        "62.5",
        "-to",
        "120",
        "-i",
        "movie.mkv",
        "-map",
        "0:a:0",
        "-vn",
        "-c:a",
        "copy",
        "-y",
        "movie.opus",
    ]


def test_output_path_never_reuses_input_path() -> None:
    assert output_path_for(Path("movie.opus"), "opus") == Path(
        "movie.audio.opus"
    )


def test_extract_rejects_non_video_without_running_ffmpeg(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mkv"
    source.touch()
    calls: list[list[str]] = []

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        calls.append(command)
        output = json.dumps(
            {"streams": [{"codec_type": "audio", "codec_name": "opus"}]}
        )
        return subprocess.CompletedProcess(command, 0, output, "")

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    with pytest.raises(VideoToAudioError, match="not a video"):
        extract_audio(source, None, None)
    assert len(calls) == 1
    assert not (tmp_path / "recording.opus").exists()


def test_extract_does_not_treat_cover_art_as_video(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mp3"
    source.touch()

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        output = json.dumps(
            {
                "streams": [
                    {"codec_type": "audio", "codec_name": "mp3"},
                    {
                        "codec_type": "video",
                        "codec_name": "mjpeg",
                        "disposition": {"attached_pic": 1},
                    },
                ]
            }
        )
        return subprocess.CompletedProcess(command, 0, output, "")

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    with pytest.raises(VideoToAudioError, match="not a video"):
        extract_audio(source, None, None)


def test_existing_output_defaults_to_no(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mkv"
    output_path = tmp_path / "recording.opus"
    source.touch()
    output_path.write_text("keep", encoding="utf-8")
    calls: list[list[str]] = []

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        calls.append(command)
        output = json.dumps(
            {
                "streams": [
                    {"codec_type": "video", "codec_name": "h264"},
                    {"codec_type": "audio", "codec_name": "opus"},
                ]
            }
        )
        return subprocess.CompletedProcess(command, 0, output, "")

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    assert extract_audio(source, None, None, lambda prompt: "") is None
    assert len(calls) == 1
    assert output_path.read_text(encoding="utf-8") == "keep"


def test_existing_output_can_be_overwritten(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mkv"
    output_path = tmp_path / "recording.opus"
    source.touch()
    output_path.touch()
    calls: list[list[str]] = []

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        calls.append(command)
        if command[0] == "ffprobe":
            output = json.dumps(
                {
                    "streams": [
                        {"codec_type": "video", "codec_name": "h264"},
                        {"codec_type": "audio", "codec_name": "opus"},
                    ]
                }
            )
            return subprocess.CompletedProcess(command, 0, output, "")
        return subprocess.CompletedProcess(command, 0)

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    assert (
        extract_audio(source, None, None, lambda prompt: "yes") == output_path
    )
    assert len(calls) == 2
    assert calls[1][0] == "ffmpeg"
