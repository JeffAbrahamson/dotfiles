#!/bin/bash

. ../script/lib.sh

dest=$HOME/.dotfiles/bash/
maybe_mkdir $dest
(
    cd bash
    for f in *; do
	copy_to $f $dest
    done
)

maybe_append .bashrc .dotfiles/bash/rc bashrc-include
maybe_append .bash_profile .dotfiles/bash/profile bash_profile-include
maybe_append .bash_logout .dotfiles/bash/logout bash_logout-include
