import os
import shutil
import subprocess
from pathlib import Path

import pytest


PROJECT_ROOT = Path(__file__).resolve().parents[1]
MD2PDF = PROJECT_ROOT / "bin" / "bin" / "md2pdf"


@pytest.mark.parametrize(
    "command",
    ["pandoc", "xelatex", "pdfinfo", "pdftotext", "fc-match"],
)
def test_md2pdf_dependency_is_available(command: str) -> None:
    assert shutil.which(command), f"md2pdf test requires {command}"


def test_md2pdf_renders_common_unicode_and_mathematics(
    tmp_path: Path,
) -> None:
    markdown = tmp_path / "unicode.md"
    pdf = tmp_path / "unicode.pdf"
    markdown.write_text(
        r"""# Font coverage

- Subscripts: Na₂CO₃ and **Na₂CO₃**
- Mathematics: $E = mc^2$ and $x_1 + y_2 = z_3$

  $$\int_0^\infty e^{-x}\,dx = 1$$

- Chinese hanzi: 中文汉字
- Japanese: 日本語の漢字、ひらがな、カタカナ
- Arrows: ← → ↔ ⇒ and **→**
- Western punctuation: “quoted text” and don’t
- Monospaced Unicode: `Na₂CO₃ → 水`
""",
        encoding="utf-8",
    )

    conversion = subprocess.run(
        [str(MD2PDF), "-D", "-o", str(pdf), str(markdown)],
        check=True,
        capture_output=True,
        text=True,
    )

    assert "Missing character" not in conversion.stderr
    extracted_text = subprocess.run(
        ["pdftotext", str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    normalized_text = extracted_text.translate(
        str.maketrans("𝐸𝑚𝑐𝑥𝑦𝑧", "Emcxyz")
    )
    compact_text = "".join(normalized_text.split())
    assert "Mathematics:E=mc2andx1+y2=z3" in compact_text
    for expected in (
        "Na₂CO₃",
        "∫",
        "中文汉字",
        "日本語の漢字、ひらがな、カタカナ",
        "← → ↔ ⇒",
        "“quoted text” and don’t",
        "Na₂CO₃ → 水",
    ):
        assert expected in extracted_text


def test_md2pdf_uses_a4_paper(tmp_path: Path) -> None:
    markdown = tmp_path / "page-size.md"
    pdf = tmp_path / "page-size.pdf"
    markdown.write_text("# A4 document\n", encoding="utf-8")

    subprocess.run(
        [str(MD2PDF), "-D", "-o", str(pdf), str(markdown)],
        check=True,
        capture_output=True,
        text=True,
    )

    metadata = subprocess.run(
        ["pdfinfo", str(pdf)],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "Page size:       595.28 x 841.89 pts (A4)" in metadata


def test_md2pdf_normalizes_nonstandard_math_delimiters(tmp_path: Path) -> None:
    markdown = tmp_path / "math-delims.md"
    pdf = tmp_path / "math-delims.pdf"
    markdown.write_text(
        r"""# Delimiters

Backslash-escaped inline: \(a_1 + b_2\)

Backslash-escaped block:

\[
c^2 = a^2 + b^2
\]

Bare block:

[
d^2 = e^2 + f^2
]
""",
        encoding="utf-8",
    )

    conversion = subprocess.run(
        [str(MD2PDF), "-D", "-o", str(pdf), str(markdown)],
        check=True,
        capture_output=True,
        text=True,
    )

    assert "Missing character" not in conversion.stderr
    extracted_text = subprocess.run(
        ["pdftotext", str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    normalized_text = extracted_text.translate(
        str.maketrans("𝑎𝑏𝑐𝑑𝑒𝑓", "abcdef")
    )
    compact_text = "".join(normalized_text.split())
    assert "a1+b2" in compact_text
    assert "c2=a2+b2" in compact_text
    assert "d2=e2+f2" in compact_text


def test_md2pdf_leaves_code_containing_bracket_like_math_untouched(
    tmp_path: Path,
) -> None:
    markdown = tmp_path / "code-vs-math.md"
    pdf = tmp_path / "code-vs-math.pdf"
    markdown.write_text(
        """# Code vs. math

```json
[
  1, 2, 3
]
```

Inline code with parens: `foo(x_{1})`
""",
        encoding="utf-8",
    )

    conversion = subprocess.run(
        [str(MD2PDF), "-D", "-o", str(pdf), str(markdown)],
        check=True,
        capture_output=True,
        text=True,
    )

    assert "Missing character" not in conversion.stderr
    extracted_text = subprocess.run(
        ["pdftotext", str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    compact_text = "".join(extracted_text.split())
    assert "[1,2,3]" in compact_text
    assert "foo(x_{1})" in compact_text


@pytest.mark.skipif(
    shutil.which("mmdc") is None,
    reason="Mermaid CLI is not installed",
)
def test_md2pdf_renders_mermaid_code_blocks(tmp_path: Path) -> None:
    markdown = tmp_path / "mermaid.md"
    pdf = tmp_path / "mermaid.pdf"
    markdown.write_text(
        """# Observation workflow

```mermaid
flowchart TD
    A["Personal observation"] --> B["Structured issue"]
    B --> C{"Related place dossier?"}
    C -->|Yes| D["Add evidence"]
    C -->|No| E["Create candidate dossier"]
    D --> F["Identify evidence gaps"]
    E --> F
    F --> G["Local verification missions"]
    G --> H{"Action threshold reached?"}
    H -->|No| F
    H -->|Yes| I["Institutional dossier"]
    I --> J["Notification and response tracking"]
```
""",
        encoding="utf-8",
    )

    conversion = subprocess.run(
        [str(MD2PDF), "-D", "-o", str(pdf), str(markdown)],
        check=True,
        capture_output=True,
        text=True,
    )

    assert "Error" not in conversion.stderr
    assert pdf.stat().st_size > 0
    metadata = subprocess.run(
        ["pdfinfo", str(pdf)],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "Pages:           1" in metadata
    extracted_text = subprocess.run(
        ["pdftotext", str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "flowchart TD" not in extracted_text


def test_md2pdf_leaves_mermaid_as_code_when_mmdc_is_missing(
    tmp_path: Path,
) -> None:
    markdown = tmp_path / "mermaid-without-cli.md"
    pdf = tmp_path / "mermaid-without-cli.pdf"
    markdown.write_text(
        """```mermaid
flowchart LR
    A --> B
```
""",
        encoding="utf-8",
    )
    environment = os.environ.copy()
    environment["PATH"] = os.pathsep.join(
        directory
        for directory in environment.get("PATH", "").split(os.pathsep)
        if not (Path(directory) / "mmdc").exists()
    )

    conversion = subprocess.run(
        [str(MD2PDF), "-D", "-o", str(pdf), str(markdown)],
        check=True,
        capture_output=True,
        text=True,
        env=environment,
    )

    assert "mmdc is not installed" in conversion.stderr
    extracted_text = subprocess.run(
        ["pdftotext", str(pdf), "-"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "flowchart LR" in extracted_text
    assert "A --> B" in extracted_text
