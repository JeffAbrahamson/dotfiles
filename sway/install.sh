#!/bin/bash

. ../script/lib.sh

host_specific_sway_config="sway/config.$(hostname -s)"
sway_config="sway/config"
if [ -r "$host_specific_sway_config" ]; then
    cat sway/config_base "$host_specific_sway_config" > "$sway_config"
else
    cat sway/config_base > "$sway_config"
fi

sway_config_dest_dir=$HOME/.config/sway
maybe_mkdir "$sway_config_dest_dir"
sway_config_dest="$sway_config_dest_dir/config"

swaylock_dest_dir="$HOME/.swaylock"
maybe_mkdir "$swaylock_dest_dir"
(cd sway/ && copy_to swaylock_config "$swaylock_dest_dir/config")

kitty_dest_dir=$HOME/.config/kitty
maybe_mkdir $kitty_dest_dir
kitty_dest="$kitty_dest_dir/kitty.conf"

if [ "X$HOSTNAME" = "Xvogel" ]; then
    (sed -e 's/{% font_size %}/13.0/;' < sway/kitty.conf > "$kitty_dest")
    (sed -e 's/{% window_title_font_size %}/10.0/; s/{% bar_font_size %}/12/;' < sway/config > "$sway_config_dest")
elif [ "X$HOSTNAME" == "Xtau-ceti" ]; then
    echo tau-ceti
    (sed -e 's/{% font_size %}/11.0/;' < sway/kitty.conf > "$kitty_dest")
    (sed -e 's/{% window_title_font_size %}/8.0/; s/{% bar_font_size %}/9/;' < sway/config > "$sway_config_dest")
elif [ "X$HOSTNAME" == "Xmorning" ]; then
    echo morning
    (sed -e 's/{% font_size %}/11.0/;' < sway/kitty.conf > "$kitty_dest")
    (sed -e 's/{% window_title_font_size %}/8.0/; s/{% bar_font_size %}/9/;' < sway/config > "$sway_config_dest")
else
    echo else
    (sed -e 's/{% font_size %}/11.0/;' < sway/kitty.conf > "$kitty_dest")
    (sed -e 's/{% window_title_font_size %}/8.0/; s/{% bar_font_size %}/9/;' < sway/config > "$sway_config_dest")
fi

dest=$HOME/bin/
maybe_mkdir $dest
(
    (
	cd sway/bin
	for f in *; do
	    copy_to $f $dest
	done
    )
)
