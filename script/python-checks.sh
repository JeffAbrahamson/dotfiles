#!/bin/bash

set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$repo_root"

mapfile -t python_targets < <(rg --files -g '*.py')

if [ "${#python_targets[@]}" -eq 0 ]; then
    echo "No Python files found." >&2
    exit 0
fi

run_black() {
    local target
    for target in "${python_targets[@]}"; do
        python3 -m black --check --required-version 24.2.0 --line-length 79 \
            --workers 1 \
            "$target"
    done
}

run_flake8() {
    python3 -m flake8 "${python_targets[@]}"
}

run_pytest() {
    PYTHONPATH="$repo_root/src:$repo_root/python" python3 -m pytest
}

case "${1:-all}" in
    black)
        run_black
        ;;
    flake8|cflake8)
        run_flake8
        ;;
    pytest)
        run_pytest
        ;;
    all)
        run_black
        run_flake8
        run_pytest
        ;;
    *)
        echo "Usage: $0 [black|flake8|cflake8|pytest|all]" >&2
        exit 1
        ;;
esac
