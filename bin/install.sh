#!/bin/bash

set -euo pipefail

. ../script/lib.sh

if [ "${1:-}" = "test" ]; then
    (
        cd ..
        python3 -m black --check --line-length 79 tests
        python3 -m flake8 tests
        python3 -m pytest
    )
    exit 0
fi

dest=$HOME/bin/
maybe_mkdir "$dest"
(
    cd bin
    for f in *; do
        [[ -f "$f" && "${f: -1}" != "~" ]] || continue
        copy_to "$f" "$dest"
    done
)

copy_to config/timezones $HOME/.timezones
