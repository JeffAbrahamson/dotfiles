#!/bin/bash

. ../script/lib.sh

maybe_mkdir $HOME/bin/
(
    cd bin
    for f in *; do
	install_to $f $HOME/bin/;
    done
)
