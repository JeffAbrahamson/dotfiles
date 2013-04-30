#!/bin/bash

if [ "X$1" = Xforce ]; then
    force="-f"
else
    force=
fi

. ../script/lib.sh

install_to_dot $force emacs
(
    cd elisp
    for f in *; do
	install_to $force $f $HOME/elisp/;
    done
)
