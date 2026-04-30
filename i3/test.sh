#!/bin/bash

set -euo pipefail

runtime_dir=$(mktemp -d)
trap 'rm -rf "$runtime_dir"' EXIT

XDG_RUNTIME_DIR="$runtime_dir" i3 -C -c i3/config
