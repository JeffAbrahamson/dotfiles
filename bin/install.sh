#!/bin/bash

if [ "X$1" = Xforce ]; then
    force=y
else
    force=n
fi

. ../script/lib.sh

maybe_mkdir $HOME/bin/
(
    cd bin
    for f in *; do
	install_to $force $f $HOME/bin/;
    done
)
