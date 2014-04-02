#!/bin/bash

. ../script/lib.sh

dest=$HOME/bin/
maybe_mkdir $dest
(
    cd bin
    for f in *; do
	copy_to $f $dest
    done
)
copy_to config/timezones $HOME/.timezones
