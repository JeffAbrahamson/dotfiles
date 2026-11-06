#!/bin/bash

## Fix system files that affect the Sway session.  Each fix is checked
## first, so sudo is only requested when a file actually needs changing.
## greetd's session command (which sets XDG_CURRENT_DESKTOP) is managed in
## the hosts repository, not here.

set -eu

## im-config 0.62 runs this hook in every login shell.  It sets no
## variables any more, but it costs about 0.4 s per shell: it starts
## about 25 grep/head processes, and a bug in logger_debug (it treats
## IM_CONFIG_VERBOSE="false" as true) makes it run systemd-cat three times.
## Editing a conffile is safe because dpkg keeps local changes on upgrade.
im_config_hook=/etc/profile.d/im-config_wayland.sh
im_config_line='. /usr/share/im-config/initializer'

if [ ! -r "$im_config_hook" ] || ! grep -qxF -e "$im_config_line" -- "$im_config_hook"; then
    exit 0
fi

echo "Disabling the im-config login-shell hook, will require sudo."
if ! sudo -n true 2>/dev/null && ! sudo -v; then
    echo "Warning: sudo not available; not changing $im_config_hook." >&2
    exit 0
fi

sudo sed -i \
    's|^\. /usr/share/im-config/initializer$|# Disabled by dotfiles sway/install-system.sh: slow, and sets nothing.\n# &|' \
    "$im_config_hook"
echo "Disabled the im-config initializer in $im_config_hook."
