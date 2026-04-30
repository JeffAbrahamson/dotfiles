# Bandwidth Tool Package

This package provides the implementation behind [`../../bin/bin/bandwidth`](../../bin/bin/bandwidth). It turns a directory of raw measurement files into structured records, tables, and summary statistics so the CLI can stay thin.

## Contents

* [`__init__.py`](__init__.py) contains the measurement loading, formatting, and reporting logic.
* [`tabular.py`](tabular.py) contains text-table rendering helpers shared by the CLI output.
