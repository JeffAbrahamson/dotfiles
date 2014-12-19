#!/bin/bash

. ../script/lib.sh

copy_to Xresources $HOME/.Xresources
if type -p xrdb >/dev/null; then
    xrdb $HOME/.Xresources
fi
