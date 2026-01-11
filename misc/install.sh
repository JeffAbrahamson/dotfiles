#!/bin/bash

. ../script/lib.sh

rg_dest=$HOME/.config/ripgrep
maybe_mkdir "${rg_dest}"

cp misc/rgignore "${rg_dest}/ignore"
