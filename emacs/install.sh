#!/bin/bash

if [ "X$1" = Xforce ]; then
    force=y
else
    force=n
fi

. ../script/lib.sh

install_to_dot $force emacs
maybe_mkdir $HOME/elisp/
(
    cd elisp
    for f in *; do
	install_to $force $f $HOME/elisp/;
    done
)
