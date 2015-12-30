#!/bin/bash

. ../script/lib.sh

dest=$HOME/.i3/
maybe_mkdir $(dirname $dest)
(cd i3/ && copy_to config $dest)
maybe_mkdir $HOME/.config/dunst
(cd i3/ && copy_to dunstrc $HOME/.config/dunst/)

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
