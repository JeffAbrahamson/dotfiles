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

p27="$dest/private-credentials-p27"
if [ ! -r "$p27" ]; then
    # This file provides p27 credentials.  It is sensitive and so not
    # in git.
    echo "Warning: p27 credentials are absent on this machine!"
else
    chmod 600 "$p27"
fi

mutt_cache_dir="$HOME/.mutt"
if [ ! -d "$mutt_cache_dir" ]; then
    mkdir "$mutt_cache_dir"
fi
chmod 700 "$mutt_cache_dir"
