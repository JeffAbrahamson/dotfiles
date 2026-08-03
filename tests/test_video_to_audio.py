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
    matroska_output_path_for,
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


def test_build_command_can_seek_after_input_using_duration() -> None:
    command = build_ffmpeg_command(
        Path("movie.mkv"),
        Path("movie.opus"),
        Decimal("62.5"),
        Decimal("120"),
        accurate_seek=True,
    )
    assert command[:7] == [
        "ffmpeg",
        "-v",
        "error",
        "-nostdin",
        "-i",
        "movie.mkv",
        "-ss",
    ]
    assert command[7:11] == ["62.5", "-t", "57.5", "-map"]


def test_accurate_matroska_command_resets_the_start_timestamp() -> None:
    command = build_ffmpeg_command(
        Path("movie.mkv"),
        Path("movie.mka"),
        Decimal("62.5"),
        Decimal("120"),
        accurate_seek=True,
    )
    offset_index = command.index("-output_ts_offset")
    assert command[offset_index + 1] == "-62.5"


def test_output_path_never_reuses_input_path() -> None:
    assert output_path_for(Path("movie.opus"), "opus") == Path(
        "movie.audio.opus"
    )
    assert matroska_output_path_for(Path("movie.mka")) == Path(
        "movie.audio.mka"
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
            if "-show_packets" in command:
                return subprocess.CompletedProcess(
                    command,
                    0,
                    json.dumps({"packets": [{"dts_time": "0"}]}),
                    "",
                )
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
    assert len(calls) == 3
    assert calls[1][0] == "ffmpeg"
    assert "-show_packets" in calls[2]


def test_extract_retries_invalid_timestamps_with_accurate_seek(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mkv"
    source.touch()
    calls: list[list[str]] = []
    packet_probe_count = 0

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        nonlocal packet_probe_count
        calls.append(command)
        if command[0] == "ffmpeg":
            return subprocess.CompletedProcess(command, 0)
        if "-show_packets" in command:
            packet_probe_count += 1
            packets = [] if packet_probe_count == 1 else [{"dts_time": "0"}]
            return subprocess.CompletedProcess(
                command, 0, json.dumps({"packets": packets}), ""
            )
        streams = [
            {"codec_type": "video", "codec_name": "h264"},
            {"codec_type": "audio", "codec_name": "opus"},
        ]
        return subprocess.CompletedProcess(
            command, 0, json.dumps({"streams": streams}), ""
        )

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    output_path = extract_audio(source, Decimal("10"), Decimal("15"))

    assert output_path == tmp_path / "recording.opus"
    ffmpeg_commands = [call for call in calls if call[0] == "ffmpeg"]
    assert ffmpeg_commands[0].index("-ss") < ffmpeg_commands[0].index("-i")
    assert ffmpeg_commands[1].index("-ss") > ffmpeg_commands[1].index("-i")
    assert "-to" not in ffmpeg_commands[1]
    assert ffmpeg_commands[1][ffmpeg_commands[1].index("-t") + 1] == "5"


def test_extract_falls_back_to_matroska(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mkv"
    source.touch()
    packet_probe_count = 0

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        nonlocal packet_probe_count
        if command[0] == "ffmpeg":
            return subprocess.CompletedProcess(command, 0)
        if "-show_packets" in command:
            packet_probe_count += 1
            packets = [{"dts_time": "0"}] if packet_probe_count == 3 else []
            return subprocess.CompletedProcess(
                command, 0, json.dumps({"packets": packets}), ""
            )
        streams = [
            {"codec_type": "video", "codec_name": "h264"},
            {"codec_type": "audio", "codec_name": "opus"},
        ]
        return subprocess.CompletedProcess(
            command, 0, json.dumps({"streams": streams}), ""
        )

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    assert extract_audio(source, Decimal("10"), Decimal("15")) == (
        tmp_path / "recording.mka"
    )


def test_explicit_matroska_uses_accurate_seek(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "recording.mkv"
    source.touch()
    ffmpeg_command: list[str] = []

    def fake_run(
        command: list[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str]:
        if command[0] == "ffmpeg":
            ffmpeg_command.extend(command)
            return subprocess.CompletedProcess(command, 0)
        if "-show_packets" in command:
            return subprocess.CompletedProcess(
                command,
                0,
                json.dumps({"packets": [{"dts_time": "0"}]}),
                "",
            )
        streams = [
            {"codec_type": "video", "codec_name": "h264"},
            {"codec_type": "audio", "codec_name": "opus"},
        ]
        return subprocess.CompletedProcess(
            command, 0, json.dumps({"streams": streams}), ""
        )

    monkeypatch.setattr(video_to_audio.subprocess, "run", fake_run)

    output_path = extract_audio(
        source, Decimal("10"), Decimal("15"), matroska=True
    )

    assert output_path == tmp_path / "recording.mka"
    assert ffmpeg_command.index("-ss") > ffmpeg_command.index("-i")
    assert "-output_ts_offset" in ffmpeg_command
