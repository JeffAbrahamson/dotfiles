#!/bin/bash

## Some functions useful in the various install/update scripts that
## manage my dotfiles.

install_to()
{
    force="$1"			# if present, should probably be "-f"
    src_name="$2"
    dst_dir="${3:-$HOME}"
    /bin/cp $force $src_name $dst_dir/
}


install_to_dot()
{
    # Prepend a "." to the name of the file when installing
    force="$1"			# if present, should probably be "-f"
    src_name="$2"
    dst_dir="${3:-$HOME}"
    /bin/cp $force $src_name $dst_dir/.$src_name
}
