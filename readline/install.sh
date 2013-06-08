#!/bin/bash

. ../script/lib.sh

maybe_mkdir $HOME/readline/
(
    cd readline
    for f in *; do
	install_to $f $HOME/bash/;
    done
)

maybe_append .inputrc readline/inputrc readline-include

