#!/bin/bash

## On at least one machine, I needed this script to run pm-suspend on lid close.

export PATH=/usr/bin:/bin

log=/tmp/lid-log.txt
date >> $log
echo "Suspending from /etc/acpi/local/lid.sh.pre." >> $log

# acpi hasn't changed the lid state yet, so run in a moment.
(sleep 2; /home/jeffab/bin/ratpoison-lid-monitor once suspend) &
