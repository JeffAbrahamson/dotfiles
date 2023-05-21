#!/bin/bash

. ../script/lib.sh

copy_to xsessionrc $HOME/.xsessionrc
if [ "X$HOSTNAME" = Xstarshine ]; then
    dpi=192
    font="xft:DejaVuSansMono:size=10"
elif [ "X$HOSTNAME" = Xvogel ]; then
    dpi=144
    font="xft:FreeMono:size=11"
else
    dpi=96
    font="9x15"
fi
cat Xresources | sed -e "s/{% dpi %}/$dpi/; s/{% font %}/$font/;" > $HOME/.Xresources

if type -p xrdb >/dev/null; then
    xrdb $HOME/.Xresources
else
    echo "xrdb is absent, any Xresources changes will be delayed to next X start."
fi
