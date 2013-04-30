#!/bin/bash

if [ "X$1" = Xforce ]; then
    force="-f"
else
    force=
fi

. ../script/lib.sh

(
    cd bin
    for f in *; do
	install_to $force $f $HOME/bin-dot/;
    done
)
