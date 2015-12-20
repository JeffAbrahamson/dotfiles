#!/bin/bash

. ../script/lib.sh

copy_to xsessionrc $HOME/.xsessionrc
if [ "X$HOSTNAME" = Xstarshine ]; then
    sed -e 's/Xft.dpi:        96/Xft.dpi:        192/;' < Xresources > $HOME/.Xresources
else
    copy_to Xresources $HOME/.Xresources
fi
if type -p xrdb >/dev/null; then
    xrdb $HOME/.Xresources
fi
