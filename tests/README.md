# Tests

This directory contains the repository's Python test suite.

## Contents

* [`conftest.py`](conftest.py) contains shared pytest fixtures and setup.
* [`test_bandwidth.py`](test_bandwidth.py) covers the bandwidth measurement library and CLI behavior.
* [`test_tsd_plot.py`](test_tsd_plot.py) covers the general time-series plotting package.
* [`test_tsd_seasonal.py`](test_tsd_seasonal.py) covers the seasonal projection and heatmap plotting logic.

## Scope

These tests focus on the more complex Python-backed tools in [`../bin/`](../bin/README.md) and [`../src/`](../src/README.md), where automated coverage pays off most.
