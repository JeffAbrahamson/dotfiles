#!/bin/bash
#
# Build the Python venv that elpy's RPC server and importmagic run in.
#
# This is the venv elpy would create itself on first use, at the path
# it expects and with the marker file it checks, so elpy accepts it
# instead of prompting to rebuild it.  emacs.el sets
# elpy-rpc-python-command to python3 to match the marker.  The venv is
# rebuilt when its interpreter stops working, as it does when a system
# upgrade removes the Python version it was made from.

set -eu

venv="$HOME/.emacs.d/elpy/rpc-venv"
marker="$venv/elpy-rpc-python-path-command"
# elpy's own RPC dependencies, plus what importmagic's server imports.
packages=(jedi flake8 autopep8 yapf black importmagic epc)
modules="jedi, flake8, autopep8, yapf, black, importmagic, epc"

if ! "$venv/bin/python" -c '' 2>/dev/null \
	|| [ "$(cat "$marker" 2>/dev/null)" != python3 ]; then
    echo "Creating the Python venv for Emacs in $venv"
    rm -rf "$venv"
    # Use the system interpreter even if a venv is active in this shell.
    /usr/bin/python3 -m venv "$venv"
    printf python3 > "$marker"
fi

if ! "$venv/bin/python" -c "import $modules" 2>/dev/null; then
    echo "Installing Python packages for Emacs: ${packages[*]}"
    "$venv/bin/python" -m pip install --quiet --upgrade "${packages[@]}"
fi
