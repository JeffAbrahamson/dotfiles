#!/bin/bash

# Some bash functions useful for python development.

# Create a new directory and initialise it for a new python project.
# This is pretty generic, it doesn't assume anything yet about what
# type of python project.
py-new-project() {
    new_proj="$1"
    if [ -z "$new_proj" ]; then
	echo "Usage: py-new-project project-name"
	return 1
    fi
    if [ ! -d "$new_proj" ]; then
	mkdir -p "$new_proj"
	cd "$new_proj"
	rsync -va "$HOME/.config/virtualenvs/new-project-template/" ./
    else
	echo "Directory '$new_proj' already exists."
    fi
}
