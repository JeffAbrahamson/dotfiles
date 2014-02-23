#!/bin/bash

. ../script/lib.sh

dest=$HOME/.notion
maybe_mkdir $dest
(
    cd notion
    for f in *; do
	copy_to $f $dest
    done
)
