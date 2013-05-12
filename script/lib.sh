#!/bin/bash

## Some functions useful in the various install/update scripts that
## manage my dotfiles.

## Copy with dated backup if necessary.
copy_to()
{
    _src_name="$1"
    if [ -d "$2" ]; then
	_dst_name="$2/$1"
    else
	_dst_name="$2"
    fi
    if ! cmp --quiet "$_src_name" "$_dst_name"; then
	# If the files are the same, we don't need to do anything.
	if [ -e "$_dst_name" ]; then
	    # Backup $_dst_file if it already exists
	    /bin/mv "$_dst_name" "$_dst_name".$(date '+%Y%m%d-%H:%M:%S')
	fi
	/bin/cp "$_src_name" "$_dst_name"
    fi
}


install_to()
{
    _src_name="$1"
    _dst_dir="${2:-$HOME}"
    copy_to $_src_name $_dst_dir/
}


install_to_dot()
{
    # Prepend a "." to the name of the file when installing
    _src_name="$1"
    _dst_dir="${2:-$HOME}"
    copy_to $_force $_src_name $_dst_dir/.$_src_name
}

maybe_mkdir()
{
    if [ ! -d "$1" ]; then
	mkdir "$1"
    fi
}

