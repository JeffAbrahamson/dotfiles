#!/bin/bash

cd $(pwd)/$(dirname $0)
(echo firewall; cd firewall && ./install.sh)
(echo bash; cd bash  && ./install.sh)
(echo bin; cd bin   && ./install.sh)
(echo emacs; cd emacs && ./install.sh)
(echo i3; cd i3 && ./install.sh)
#(echo irsii; cd irssi && ./install.sh)
#(echo notion; cd notion && ./install.sh)
#(echo ratpoison; cd ratpoison && ./install.sh)
(echo readline; cd readline && ./install.sh)
(echo screen; cd screen && ./install.sh)
(echo postgresql; cd postgresql && ./install.sh)
(echo tmux; cd tmux && ./install.sh)
#(echo templates; cd templates && ./install.sh)
(echo X11; cd X11 && ./install.sh)
if [ "X$HOSTNAME" = Xstarshine ]; then
    (echo touchegg; cd touchegg && ./install.sh)
fi
(echo mutt; cd mutt && ./install.sh)
