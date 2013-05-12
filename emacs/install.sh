#!/bin/bash

. ../script/lib.sh

install_to_dot emacs
maybe_mkdir $HOME/elisp/
(
    cd elisp
    for f in *; do
	install_to $f $HOME/elisp/;
    done
)
