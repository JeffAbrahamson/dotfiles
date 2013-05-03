#!/bin/bash

if [ "X$1" = Xforce ]; then
    force=y
else
    force=n
fi

. ../script/lib.sh

#install_to_dot $force bashrc
#install_to_dot $force bash_profile
#install_to_dot $force bash_logout
maybe_mkdir $HOME/bash/
(
    cd bash
    for f in *; do
	install_to $force $f $HOME/bash/;
    done
)


maybe_append()
{
    include_from=$1
    include_this=$2
    include_template=$3
    grep -q HOME/$include_this $HOME/$include_from 2> /dev/null
    if [ 0 != $? ]; then
	cat $include_template >> $HOME/$include_from
    fi
}

maybe_append .bashrc bash/rc bashrc-include
maybe_append .bash_profile bash/profile bash_profile-include
maybe_append .bash_logout bash/logout bash_logout-include
