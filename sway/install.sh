#!/bin/bash

. ../script/lib.sh

dest=$HOME/.config/sway
maybe_mkdir $dest
(cd sway/ && copy_to config $dest)

swaylock_dest="$HOME/.swaylock"
maybe_mkdir "$swaylock_dest"
(cd sway/ && copy_to swaylock_config "$swaylock_dest/config")

dest=$HOME/.config/kitty
maybe_mkdir $dest
(cd sway/ && copy_to kitty.conf $dest)

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
