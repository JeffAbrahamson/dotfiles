#!/bin/bash

cd $(pwd)/$(dirname $0)
# In case this is the first time I'm installing my dotfiles on this
# host, make sure I've got a firewall first and foremost.
(echo firewall; cd firewall && ./install.sh)

(echo X11; cd X11 && ./install.sh)
(echo bash; cd bash  && ./install.sh)
(echo bin; cd bin   && ./install.sh)
(echo emacs; cd emacs && ./install.sh)
(echo i3; cd i3 && ./install.sh)
(echo postgresql; cd postgresql && ./install.sh)
(echo python; cd python && ./install.sh)
(echo readline; cd readline && ./install.sh)
(echo screen; cd screen && ./install.sh)
(echo sway; cd sway && ./install.sh)
(echo tmux; cd tmux && ./install.sh)

# Things I no longer use but might some day.
#
# (echo irsii; cd irssi && ./install.sh)
# (echo notion; cd notion && ./install.sh)
# (echo ratpoison; cd ratpoison && ./install.sh)
# if [ "X$HOSTNAME" = Xstarshine ]; then
#     (echo touchegg; cd touchegg && ./install.sh)
# fi
# (echo mutt; cd mutt && ./install.sh)

./cleanup.sh
