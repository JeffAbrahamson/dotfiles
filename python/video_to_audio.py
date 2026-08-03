"""Extract a video's first audio stream without re-encoding it."""

from __future__ import annotations

import json
import subprocess
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Callable, Sequence


CODEC_EXTENSIONS = {
    "aac": ".m4a",
    "ac3": ".ac3",
    "alac": ".m4a",
    "dts": ".dts",
    "eac3": ".eac3",
    "flac": ".flac",
    "mp3": ".mp3",
    "opus": ".opus",
    "truehd": ".thd",
    "vorbis": ".ogg",
}


class VideoToAudioError(Exception):
    """An error that can be reported directly to the user."""


def parse_time(value: str) -> Decimal:
    """Parse non-negative seconds, MM:SS, or HH:MM:SS."""
    original = value
    fields = value.split(":")
    if len(fields) not in (1, 2, 3) or any(not field for field in fields):
        raise ValueError(original)

    try:
        numbers = [Decimal(field) for field in fields]
    except InvalidOperation as exc:
        raise ValueError(original) from exc

    if any(not number.is_finite() or number < 0 for number in numbers):
        raise ValueError(original)
    if len(numbers) > 1:
        if any(number != number.to_integral() for number in numbers[:-1]):
            raise ValueError(original)
        if numbers[-1] >= 60:
            raise ValueError(original)
    if len(numbers) == 3 and numbers[1] >= 60:
        raise ValueError(original)

    if len(numbers) == 1:
        return numbers[0]
    if len(numbers) == 2:
        return numbers[0] * 60 + numbers[1]
    return numbers[0] * 3600 + numbers[1] * 60 + numbers[2]


def format_time(value: Decimal) -> str:
    """Return a plain decimal representation suitable for ffmpeg."""
    return format(value, "f")


def probe_streams(input_path: Path) -> list[dict[str, Any]]:
    """Return the media streams reported by ffprobe."""
    try:
        result = subprocess.run(
            [
                "ffprobe",
                "-v",
                "error",
                "-show_entries",
                "stream=codec_type,codec_name:stream_disposition=attached_pic",
                "-of",
                "json",
                str(input_path),
            ],
            check=False,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError as exc:
        raise VideoToAudioError("ffprobe was not found in PATH") from exc

    if result.returncode != 0:
        detail = result.stderr.strip()
        message = f"Input is not a video: {input_path}"
        if detail:
            message += f" (ffprobe could not read it: {detail})"
        raise VideoToAudioError(message)

    try:
        streams = json.loads(result.stdout)["streams"]
    except (json.JSONDecodeError, KeyError, TypeError) as exc:
        raise VideoToAudioError(
            f"Cannot inspect {input_path}: invalid ffprobe output"
        ) from exc
    return streams


def inspect_video(input_path: Path) -> str:
    """Validate the input and return its first audio codec."""
    if not input_path.is_file():
        raise VideoToAudioError(f"Input file does not exist: {input_path}")

    streams = probe_streams(input_path)
    if not any(
        stream.get("codec_type") == "video"
        and (stream.get("disposition") or {}).get("attached_pic", 0) != 1
        for stream in streams
    ):
        raise VideoToAudioError(f"Input is not a video: {input_path}")

    audio_streams = [
        stream for stream in streams if stream.get("codec_type") == "audio"
    ]
    if not audio_streams:
        raise VideoToAudioError(f"Video has no audio stream: {input_path}")

    codec = audio_streams[0].get("codec_name")
    if not codec:
        raise VideoToAudioError(
            f"Cannot determine the audio codec in {input_path}"
        )
    return codec


def output_path_for(input_path: Path, codec: str) -> Path:
    """Choose a conventional extension that can hold the copied codec."""
    extension = CODEC_EXTENSIONS.get(codec)
    if codec.startswith("pcm_"):
        extension = ".wav"
    if extension is None:
        extension = ".mka"
    output_path = input_path.with_suffix(extension)
    if output_path == input_path:
        output_path = input_path.with_name(
            f"{input_path.stem}.audio{extension}"
        )
    return output_path


def confirm_overwrite(
    output_path: Path, input_func: Callable[[str], str] = input
) -> bool:
    """Ask before replacing an existing output, defaulting to no."""
    try:
        answer = input_func(
            f"Output file already exists: {output_path}. Overwrite? [y/N] "
        )
    except EOFError:
        return False
    return answer.strip().lower() in ("y", "yes")


def build_ffmpeg_command(
    input_path: Path,
    output_path: Path,
    start: Decimal | None,
    end: Decimal | None,
) -> list[str]:
    """Build the stream-copy command."""
    command = ["ffmpeg", "-v", "error", "-nostdin"]
    if start is not None:
        command.extend(["-ss", format_time(start)])
    if end is not None:
        command.extend(["-to", format_time(end)])
    command.extend(
        [
            "-i",
            str(input_path),
            "-map",
            "0:a:0",
            "-vn",
            "-c:a",
            "copy",
            "-y",
            str(output_path),
        ]
    )
    return command


def extract_audio(
    input_path: Path,
    start: Decimal | None,
    end: Decimal | None,
    input_func: Callable[[str], str] = input,
) -> Path | None:
    """Validate and extract audio, returning None if overwrite is declined."""
    codec = inspect_video(input_path)
    output_path = output_path_for(input_path, codec)

    if output_path.exists() and not confirm_overwrite(output_path, input_func):
        return None

    command = build_ffmpeg_command(input_path, output_path, start, end)
    try:
        result = subprocess.run(command, check=False)
    except FileNotFoundError as exc:
        raise VideoToAudioError("ffmpeg was not found in PATH") from exc
    if result.returncode != 0:
        raise VideoToAudioError(f"ffmpeg failed while creating {output_path}")
    return output_path


def parse_optional_times(
    values: Sequence[str],
) -> tuple[Decimal | None, Decimal | None]:
    """Parse optional start and end arguments with useful error messages."""
    parsed: list[Decimal] = []
    labels = ("start", "end")
    for label, value in zip(labels, values):
        try:
            parsed.append(parse_time(value))
        except ValueError as exc:
            raise VideoToAudioError(
                f"Unparseable {label} time {value!r}; expected seconds, "
                "MM:SS, or HH:MM:SS"
            ) from exc

    start = parsed[0] if parsed else None
    end = parsed[1] if len(parsed) == 2 else None
    if start is not None and end is not None and end <= start:
        raise VideoToAudioError(
            f"End time ({values[1]!r}) must be later than start time "
            f"({values[0]!r})"
        )
    return start, end
