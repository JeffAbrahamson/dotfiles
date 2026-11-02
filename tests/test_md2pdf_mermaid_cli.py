import os
import subprocess
import sys
from pathlib import Path

import pytest

FILTER = Path(__file__).resolve().parents[1] / "bin/bin/md2pdf-mermaid.lua"


@pytest.mark.parametrize("legacy_pdf_fit", [False, True])
def test_mermaid_pdf_fitting_options(tmp_path, legacy_pdf_fit):
    cli = tmp_path / "mmdc"
    cli.write_text(
        f"#!{sys.executable}\n"
        "import sys\n"
        "from pathlib import Path\n"
        f"legacy = {legacy_pdf_fit!r}\n"
        "if sys.argv[1:] == ['--help']:\n"
        "    print('--pdfFit' if legacy else '--pdf-paper-format')\n"
        "else:\n"
        "    assert ('--pdfFit' in sys.argv) == legacy\n"
        "    output = sys.argv[sys.argv.index('--output') + 1]\n"
        "    Path(output).write_bytes(b'%PDF-1.4\\n')\n"
    )
    cli.chmod(0o755)
    environment = os.environ.copy()
    environment["PATH"] = f"{tmp_path}{os.pathsep}{environment['PATH']}"

    conversion = subprocess.run(
        ["pandoc", "--from=markdown", "--to=json", f"--lua-filter={FILTER}"],
        input="```mermaid\nflowchart LR\nA --> B\n```\n",
        text=True,
        capture_output=True,
        check=True,
        env=environment,
    )

    assert '"Image"' in conversion.stdout
    assert "mermaid-" in conversion.stdout
