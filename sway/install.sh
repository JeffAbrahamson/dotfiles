#!/bin/bash

. ../script/lib.sh

dest=$HOME/.config/sway
maybe_mkdir $dest
maybe_mkdir $(dirname $dest)
(cd sway/ && copy_to config $dest)

dest=$HOME/bin/
maybe_mkdir $dest
(
    (
	cd sway/bin
	for f in *; do
	    copy_to $f $dest
	done
    )
)
