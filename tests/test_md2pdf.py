import shutil
import subprocess
from pathlib import Path

import pytest


PROJECT_ROOT = Path(__file__).resolve().parents[1]
MD2PDF = PROJECT_ROOT / "bin" / "bin" / "md2pdf"


@pytest.mark.parametrize(
    "command",
    ["pandoc", "xelatex", "pdftotext", "fc-match"],
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
