#!/bin/bash

. ../script/lib.sh

maybe_mkdir $HOME/elisp/
(
    cd elisp
    for f in *; do
	install_to $f $HOME/elisp/;
    done
)

maybe_append .emacs /elisp/emacs.el emacs-include
