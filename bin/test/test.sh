#!/bin/bash

echo 'bin test.sh not yet implemented for most scripts.'
for file in *; do
    if [ -x "$file" -a "$file" != test.sh -a "${file: -1}" != "~" ]; then
	echo "  Running $file."
	./$file
    fi
done
