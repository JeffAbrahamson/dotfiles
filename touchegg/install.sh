#!/bin/bash

. ../script/lib.sh

dest=$HOME/.config/touchegg/
maybe_mkdir $(dirname $dest)
copy_to touchegg/touchegg.conf $dest

