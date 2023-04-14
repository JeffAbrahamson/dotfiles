#!/bin/bash

. ../script/lib.sh

dest=$HOME/.dotfiles/elisp/
maybe_mkdir $dest
(
    cd elisp
    for f in *; do
	copy_to $f $dest
    done
)

maybe_mkdir ${dest}/dist
rsync lib/copilot-dist/* ${dest}/dist/

maybe_append .emacs .dotfiles/elisp/emacs.el emacs-include
