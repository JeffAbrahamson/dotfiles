#!/bin/bash

. ../script/lib.sh

host_specific_config="i3/config.$(hostname -s)"
if [ -r "$host_specific_config" ]; then
    cat i3/config_base "$host_specific_config" > i3/config
else
    cat i3/config_base > i3/config
fi
in
dest=$HOME/.i3/
maybe_mkdir $dest
(cd i3/ && copy_to i3status.conf $HOME/.i3status.conf)
maybe_mkdir $(dirname $dest)
(cd i3/ && copy_to config $dest)
maybe_mkdir $HOME/.config/dunst
if [ "X$HOSTNAME" == "Xbirdsong" -o "X$HOSTNAME" = "Xmorning" ]; then
    (cd i3/ && sed -e 's/{% fontsize %}/10/;' < dunstrc > $HOME/.config/dunst/dunstrc)
elif [ "X$HOSTNAME" == "Xvogel" ]; then
    (cd i3/ && sed -e 's/{% fontsize %}/16/;' < dunstrc > $HOME/.config/dunst/dunstrc)
elif [ "X$HOSTNAME" == "Xstarshine" ]; then
    (cd i3/ && sed -e 's/{% fontsize %}/18/;' < dunstrc > $HOME/.config/dunst/dunstrc)
elif [ "X$HOSTNAME" == "Xtau-ceti" ]; then
    (cd i3/ && sed -e 's/{% fontsize %}/11/;' < dunstrc > $HOME/.config/dunst/dunstrc)
else
    echo "Unrecognized host, not copying dunstrc."
fi

dest=$HOME/bin/
maybe_mkdir $dest
(
    (
	cd i3/bin
	for f in *; do
	    copy_to $f $dest
	done
    )
)
