#!/bin/bash

. ../script/lib.sh

dest=$HOME/templates/
maybe_mkdir $dest
(
    # Install the template files
    cd templates
    for f in *; do
	copy_to $f $dest
    done
)

(
    # If I have private template files, install them as well.
    # $HOME/common/ is where I mount a remote file system of files I
    # want on all my machines.
    templates=$HOME/common/templates
    echo "  checking $templates..."
    if [ -d $templates ]; then
	cd $templates
	for f in *; do
	    copy_to $f $dest
	done
    fi
)
