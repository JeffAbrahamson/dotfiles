#!/bin/bash

set -euo pipefail

echo 'Running bin smoke tests.'
for file in *; do
    if [ -f "$file" ] && [ -x "$file" ] && [ "$file" != test.sh ] && [ "${file: -1}" != "~" ]; then
        echo "  Running $file."
        ./$file
    fi
done
