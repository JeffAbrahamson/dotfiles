#!/bin/bash

. ../script/lib.sh

dest=$HOME/.dotfiles/readline/
maybe_mkdir $dest
(
    cd readline
    for f in *; do
	copy_to $f $dest
    done
)

maybe_append .inputrc .dotfiles/readline/inputrc readline-include

