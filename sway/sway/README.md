# Sway Config

This directory contains the main Sway configuration, notification setup, lock-screen config, and helper scripts. It is the active Wayland-era counterpart to the repo's older X11/i3-style desktop configuration.

## Contents

* [`config`](config), [`config_base`](config_base), and host-specific variants such as [`config.morning`](config.morning) and [`config.vogel`](config.vogel) define the Sway setup.
* [`kitty.conf`](kitty.conf), [`mako-config`](mako-config), [`swaylock_config`](swaylock_config), and [`systemd-mako-service`](systemd-mako-service) configure surrounding desktop components.
* [`bin/`](bin/README.md) contains helper scripts used by keybindings and background tasks.
