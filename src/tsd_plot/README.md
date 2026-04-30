# tsd_plot

This package implements the heavier plotting logic used by [`../../bin/bin/tsd-plot.py`](../../bin/bin/tsd-plot.py) and [`../../bin/bin/tsd-season-plot.py`](../../bin/bin/tsd-season-plot.py). It keeps the CLIs fairly small while centralizing parsing, aggregation, and plotting behavior in importable modules.

## Contents

* [`cli.py`](cli.py) implements general time-series plotting, binning, and aggregation.
* [`seasonal.py`](seasonal.py) implements seasonal projections and recurring-pattern heatmaps for yearly or weekly views.
* [`__init__.py`](__init__.py) marks the package and exposes the module namespace.
