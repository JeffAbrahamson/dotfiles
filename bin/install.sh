#!/bin/bash

set -euo pipefail

. ../script/lib.sh

if [ "${1:-}" = "test" ]; then
    (
        cd ..
        python3 -m black --check --line-length 79 \
            bin/bin/tsd-plot.py bin/bin/tsd-season-plot.py \
            src/tsd_plot tests
        python3 -m flake8 \
            bin/bin/tsd-plot.py bin/bin/tsd-season-plot.py \
            src/tsd_plot tests
        PYTHONPATH=src python3 -m pytest
    )
    exit 0
fi

dest=$HOME/bin/
maybe_mkdir "$dest"
(
    cd bin
    for f in *; do
        [[ -f "$f" && "${f: -1}" != "~" ]] || continue
        case "$f" in
            tsd-plot.py|tsd-season-plot.py)
                copy_to "$f" "$dest/${f%.py}"
                ;;
            *)
                copy_to "$f" "$dest"
                ;;
        esac
    done
)

site_packages=$(python3 - <<'PY'
import site
print(site.getusersitepackages())
PY
)

maybe_mkdir "$site_packages/tsd_plot"
rsync -av --delete ../src/tsd_plot/ "$site_packages/tsd_plot/"

copy_to config/timezones $HOME/.timezones
