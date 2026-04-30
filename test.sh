#!/bin/bash

set -euo pipefail

for dir in */; do
    test_script="${dir%/}/test.sh"
    if [ -x "$test_script" ]; then
        (
            cd "${dir%/}"
            ./test.sh
        )
    fi
done

./script/python-checks.sh all
