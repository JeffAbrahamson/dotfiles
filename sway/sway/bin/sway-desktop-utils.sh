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
    #
    # IMPORTANT: We must wait for swayidle to actually die before returning.
    # If called from an after-resume handler, we're running as a child of
    # swayidle.  A simple pkill sends SIGTERM but the parent swayidle may be
    # blocked in wait() for us to finish.  If we then exec a new swayidle
    # before the parent dies, we end up with nested swayidle processes, and
    # the parent's suspend timeout will fire unexpectedly.
    pkill -u $LOGNAME swayidle

    _err=$?
    if [ $_err = 0 ]; then
	log_line "Kill status: SIGTERM sent, waiting for swayidle to die..."
	# Wait for all swayidle processes to actually terminate.
	# The parent may be our ancestor, so it can't die until we exec,
	# but we need to give it time to at least receive the signal and
	# prepare to exit once we do.
	for _i in $(seq 20); do
	    # Check if any swayidle is still running (excluding zombies)
	    if ! pgrep -u $LOGNAME swayidle > /dev/null 2>&1; then
		log_line "Kill status: swayidle terminated."
		return 0
	    fi
	    sleep 0.1
	done
	# Still alive after 2 seconds - use SIGKILL
	log_line "Kill status: swayidle still alive, sending SIGKILL..."
	pkill -9 -u $LOGNAME swayidle
	sleep 0.2
	log_line "Kill status: done."
    else
	log_line "Kill status: no swayidle found ($_err)."
    fi
}
