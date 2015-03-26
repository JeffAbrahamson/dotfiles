#!/bin/bash

. ../script/lib.sh

copy_to Xresources $HOME/.Xresources
copy_to xsessionrc $HOME/.xsessionrc
if type -p xrdb >/dev/null; then
    xrdb $HOME/.Xresources
fi
