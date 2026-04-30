# Docker Support Files

This directory contains helper scripts used by the Docker image and container entrypoint.

## Contents

* [`entrypoint.sh`](entrypoint.sh) prepares the container at startup.
* [`packages-dev.sh`](packages-dev.sh) installs development dependencies needed for checks and editing.
* [`packages-runtime.sh`](packages-runtime.sh) installs the smaller runtime dependency set.
