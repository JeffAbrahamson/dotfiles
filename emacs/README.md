# Emacs

This directory contains the Emacs configuration and local elisp used by the repo owner. It mixes general editor setup with a library of personal helper functions and bundled third-party mode files.

## Contents

* [`elisp/`](elisp/README.md) holds the elisp source files.
* [`lib/`](lib/README.md) documents the package layout conventions used by installed libraries.
* [`emacs-include`](emacs-include) is the top-level include file installed into the home directory.
* [`Makefile`](Makefile) provides the `install` and `test` targets for the Emacs config. `install` also runs [`install-packages.el`](install-packages.el), which installs missing ELPA packages, and [`install-python-venv.sh`](install-python-venv.sh), which builds the venv that elpy and importmagic run in. Both need the network; start-up itself doesn't.
* In a sway session, Emacs runs as a daemon started by the sway config; `ec` and `emf` open frames from it on the current directory.
