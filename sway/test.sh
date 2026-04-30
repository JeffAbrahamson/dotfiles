#!/bin/bash

set -euo pipefail

runtime_dir=$(mktemp -d)
trap 'rm -rf "$runtime_dir"' EXIT

if ! output=$(XDG_RUNTIME_DIR="$runtime_dir" sway -C -c sway/config 2>&1); then
    case "$output" in
        *"Unable to create backend"*|*"Could not connect to remote display"*)
            echo "Skipping sway config validation: no display backend available."
            exit 0
            ;;
    esac
    printf '%s\n' "$output" >&2
    exit 1
fi
