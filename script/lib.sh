#!/bin/bash

## Some functions useful in the various install/update scripts that
## manage my dotfiles.

install_to()
{
    _force="$1"			# y or n
    _src_name="$2"
    _dst_dir="${3:-$HOME}"
    if [ $_force = y ]; then
	_force=-f
    else
	_force=
    fi
    /bin/cp $_force $_src_name $_dst_dir/
}


install_to_dot()
{
    # Prepend a "." to the name of the file when installing
    _force="$1"			# y or n
    _src_name="$2"
    _dst_dir="${3:-$HOME}"
    if [ $_force = y ]; then
	_force=-f
    else
	_force=
    fi
    /bin/cp $_force $_src_name $_dst_dir/.$_src_name
}

maybe_mkdir()
{
    if [ ! -d "$1" ]; then
	mkdir "$1"
    fi
}

