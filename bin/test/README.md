# Bin Tests

This directory contains tests and fixtures for scripts under [`../bin/`](../bin/README.md). The mix of shell and Python tests reflects the mix of implementation styles in the executable directory.

## Contents

* [`test.sh`](test.sh) is the main entry point for the bin-specific test run.
* [`bandwidth-history-test/`](bandwidth-history-test) provides fixture files for bandwidth history processing.
* [`tsd-season-plot-test`](tsd-season-plot-test) exercises the seasonal plotting CLI.
* [`tsd-time-to-empty-test`](tsd-time-to-empty-test) covers the forecasting and reporting logic in `tsd-time-to-empty.py`.
* [`compare-sum-lists`](compare-sum-lists) and related golden files cover the checksum comparison tool.
