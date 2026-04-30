#!/bin/bash

set -euo pipefail

python_targets=(
    bin/bin
    src
    tests
    python
)

run_black() {
    python3 -m black --check --workers 1 --line-length 79 \
        "${python_targets[@]}"
}

run_flake8() {
    python3 -m flake8 "${python_targets[@]}"
}

run_pytest() {
    PYTHONPATH=src:python python3 -m pytest
}

case "${1:-all}" in
    black)
        run_black
        ;;
    flake8)
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
        echo "Usage: $0 [black|flake8|pytest|all]" >&2
        exit 1
        ;;
esac
