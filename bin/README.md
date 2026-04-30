# Bin

This directory collects standalone command-line tools plus their tests and supporting configuration. It is the main "utilities" area of the repository, ranging from tiny wrappers to more involved analysis scripts.

## Contents

* [`bin/`](bin/README.md) contains the executable programs.
* [`config/`](config/README.md) stores small support data files used by some scripts.
* [`test/`](test/README.md) contains fixture data and script-level tests.
* [`install.sh`](install.sh) installs the executables.
* [`test.sh`](test.sh) runs the bin-specific test suite.

## More complex tools

* [`bin/bandwidth`](bin/bandwidth) reads recorded network measurements and renders either compact tables or richer summary views, which makes it easier to inspect connection quality over time.
* [`bin/tsd-plot.py`](bin/tsd-plot.py), [`bin/tsd-season-plot.py`](bin/tsd-season-plot.py), [`bin/tsd-time-to-empty.py`](bin/tsd-time-to-empty.py), [`bin/tsd-bicycle`](bin/tsd-bicycle), and [`bin/tsd-sleep`](bin/tsd-sleep) are data-analysis tools built around time-series data; they help turn personal logs into plots, seasonal views, or forecasts instead of forcing one-off notebook work.
* [`bin/bandwidth_history.py`](bin/bandwidth_history.py) is an older plotting-oriented bandwidth tool for historical datasets and complements the newer library-backed `bandwidth` CLI.
