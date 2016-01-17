#!/bin/bash

. ../script/lib.sh

copy_to xsessionrc $HOME/.xsessionrc
if [ "X$HOSTNAME" = Xstarshine ]; then
    # On a HiDPI display, set dpi high.
    # And use a font with urxvt that works better at HiDPI.
    sed -e 's/Xft.dpi:        96/Xft.dpi:        192/; s/urxvt.font: 9x15/urxvt.font: xft:DejaVuSansMono:size=10/;' < Xresources > $HOME/.Xresources
else
    copy_to Xresources $HOME/.Xresources
fi
if type -p xrdb >/dev/null; then
    xrdb $HOME/.Xresources
fi
