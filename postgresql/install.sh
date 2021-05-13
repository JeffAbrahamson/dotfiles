#!/bin/bash

. ../script/lib.sh

dest=$HOME/.psqlrc
copy_to postgresql/rc $dest
