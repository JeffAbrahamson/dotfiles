#!/bin/bash

. ../script/lib.sh

maybe_mkdir $HOME/bash/
(
    cd bash
    for f in *; do
	install_to $f $HOME/bash/;
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
