#!/bin/bash

. ../script/lib.sh

if [ "X$HOSTNAME" = Xvogel ]; then
    # dpi=144
    dpi=108
else
    dpi=96
fi
cat Xdefaults  | sed -e "s/{% dpi %}/$dpi/;" > $HOME/.Xdefaults
