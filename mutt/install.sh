#!/bin/bash

. ../script/lib.sh

dest=$HOME/.dotfiles/mutt/
maybe_mkdir "$dest"
chmod 700 "$dest"
copy_to muttrc $HOME/.muttrc
(
    cd mutt
    for f in *; do
	copy_to $f $dest
    done
)
(
    cd bin
    for f in *; do
	copy_to $f $HOME/bin/
    done
)

secrets="$dest/secrets"
if [ ! -r "$secrets" ]; then
    # This file provides credentials.  It is sensitive and so not
    # in git.
    echo "Warning: secret credentials are absent on this machine!"
else
    chmod 600 "$secrets"
fi

mutt_cache_dir="$HOME/.mutt"
if [ ! -d "$mutt_cache_dir" ]; then
    mkdir "$mutt_cache_dir"
fi
chmod 700 "$mutt_cache_dir"
