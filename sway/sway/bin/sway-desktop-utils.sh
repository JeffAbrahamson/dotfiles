#!/bin/bash

# Common functions for logging to sway-desktop idle support.

. $HOME/bin/p27.sh
PATH=/usr/bin:/bin:$HOME/bin
log_to_file "$HOME/sway-desktop-idle" quiet
log_line "$0 $*"

only_desktop_host() {
    if [ "X$HOSTNAME" != Xvogel ]; then
	log_line "Host '$HOSTNAME' not recognised, not deactivating screens."
	exit 0
    fi
}

kill_idle_with_status() {
    # My intention is to replace any existing swayidle process I'm running.
    pkill -u $LOGNAME swayidle

    # Assume standard C convention: 0 is success, anything else is
    # failure.
    _err=$?
    if [ $_err = 0 ]; then
	log_line "Kill status: succeeded."
    else
	log_line "Kill status: failed ($_err)."
    fi
}
