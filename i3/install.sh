#!/bin/bash

. ../script/lib.sh

dest=$HOME/.i3/
copy_to i3/config $dest

dest=$HOME/bin/
maybe_mkdir $dest
(
    (
	cd i3/bin
	for f in *; do
	    copy_to $f $dest
	done
    )
)
