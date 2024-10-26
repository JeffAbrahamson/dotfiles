#!/bin/bash

. ../script/lib.sh

/bin/cp -f python/flake8 $HOME/.flake8

bash_dest=$HOME/.dotfiles/bash/rc_post/
maybe_mkdir $bash_dest
rsync -av bash/ $bash_dest/

np_dest=$HOME/.config/virtualenvs/new-project-template/
maybe_mkdir "$np_dest"
rsync -av --delete new-project-template/ "$np_dest/"
