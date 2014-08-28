#!/bin/bash

. ../script/lib.sh

password=$(cat $HOME/.irssi/password)
sed "s/{{password}}/$password/" < irssi/startup.in > irssi/startup
chmod 400 irssi/startup

dest=$HOME/.irssi/
maybe_mkdir $dest
(
    cd irssi
    for f in *; do
	copy_to $f $dest
    done
)

/bin/rm -f irssi/startup
