#!/bin/bash

. ../script/lib.sh

copy_to Xresources $HOME/.Xresources
xrdb $HOME/.Xresources
