#!/bin/bash

. ../script/lib.sh

dest=$HOME/.i3/config
maybe_mkdir $(dirname $dest)
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
