"""Minimal tabular rendering helpers used by CLI tools.

This module provides a lightweight alternative to third-party table
renderers so the repository does not depend on external packages at run
or test time.  The :func:`format_table` helper aligns text in a way that is
compatible with traditional command line utilities such as ``column``.
"""
from __future__ import annotations

from typing import Iterable, List, Sequence

Alignment = Sequence[str]
Row = Sequence[str]


def _align_cell(text: str, width: int, align: str) -> str:
    if align == "right":
        return text.rjust(width)
    if align == "center":
        return text.center(width)
    # Default to left alignment
    return text.ljust(width)


def format_table(headers: Sequence[str], rows: Iterable[Row], *, colalign: Alignment | None = None) -> str:
    """Return a string representing a table with aligned columns.

    Parameters
    ----------
    headers:
        An iterable of column headers.
    rows:
        An iterable containing row sequences.  Each value is expected to be a
        string that is already formatted for display.
    colalign:
        Optional iterable describing the alignment for each column.  Valid
        values are ``"left"``, ``"right"`` and ``"center"``.  When omitted, the
        header alignment defaults to left alignment.
    """

    row_list: List[Row] = [tuple(row) for row in rows]
    num_cols = len(headers)

    if colalign is None:
        colalign = tuple("left" for _ in range(num_cols))
    elif len(colalign) != num_cols:
        raise ValueError("column alignment must match number of headers")

    widths = [len(headers[i]) for i in range(num_cols)]
    for row in row_list:
        if len(row) != num_cols:
            raise ValueError("row has different number of columns than headers")
        for i, cell in enumerate(row):
            widths[i] = max(widths[i], len(cell))

    header_line = "  ".join(
        _align_cell(headers[i], widths[i], colalign[i]) for i in range(num_cols)
    )
    divider = "  ".join("-" * widths[i] for i in range(num_cols))

    body_lines = [
        "  ".join(_align_cell(cell, widths[i], colalign[i]) for i, cell in enumerate(row))
        for row in row_list
    ]

    lines = [header_line, divider, *body_lines]
    return "\n".join(lines)
