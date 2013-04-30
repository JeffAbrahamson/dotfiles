#!/bin/bash

if [ "X$1" = Xforce ]; then
    force="-f"
else
    force=
fi

. ../script/lib.sh

install_to_dot $force bashrc
install_to_dot $force bash_profile
install_to_dot $force bash_logout
(
    cd bash
    for f in *; do
	install_to $force $f $HOME/bash/;
    done
)
