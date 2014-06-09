#!/bin/bash

. ../script/lib.sh

dest=$HOME/.irssi/
maybe_mkdir $dest
(
    cd irssi
    for f in *; do
	copy_to $f $dest
    done
)
