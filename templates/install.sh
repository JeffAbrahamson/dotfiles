#!/bin/bash

. ../script/lib.sh

maybe_mkdir $HOME/templates
(
    # Install the template files
    cd templates
    for f in *; do
	install_to $f $HOME/templates/;
    done
)

(
    # If I have private template files, install them as well
    templates=$HOME/common/templates
    echo "  checking $templates..."
    if [ -d $templates ]; then
	cd $templates
	for f in *; do
	    install_to $f $HOME/templates;
	done
    fi
)
