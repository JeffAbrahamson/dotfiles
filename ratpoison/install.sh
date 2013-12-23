#!/bin/bash

. ../script/lib.sh

dest=$HOME/.ratpoisonrc
copy_to ratpoison/rc $dest

desktop_entry=/usr/share/xsessions/ratpoison.desktop
my_desktop_entry=$(pwd)/ratpoison/ratpoison.desktop
if [ ! -f $desktop_entry ]; then
    echo "In order to start ratpoison at login, consider"
    echo "sudo cp $my_desktop_entry $desktop_entry"
elif ! cmp --quiet "$my_desktop_entry" "$desktop_entry"; then
    echo "The system ratpoison desktop entry has changed.  Consider"
    echo "diff $my_desktop_entry $desktop_entry"
    echo "sudo cp $my_desktop_entry $desktop_entry"
fi

dest=$HOME/bin/
maybe_mkdir $dest
(
    (
	cd ratpoison/bin
	for f in *; do
	    copy_to $f $dest
	done
    )
)
