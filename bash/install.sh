#!/bin/bash

. ../script/lib.sh

maybe_mkdir $HOME/bash/
(
    cd bash
    for f in *; do
	install_to $f $HOME/bash/;
    done
)

maybe_append .bashrc bash/rc bashrc-include
maybe_append .bash_profile bash/profile bash_profile-include
maybe_append .bash_logout bash/logout bash_logout-include
