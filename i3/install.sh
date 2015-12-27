#!/bin/bash

. ../script/lib.sh

dest=$HOME/.i3/
maybe_mkdir $(dirname $dest)
(cd i3/ && copy_to config $dest)
(cd i3/ && copy_to dunstrc $dest)

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
