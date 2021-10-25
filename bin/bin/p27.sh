#!/bin/bash

# Common functions for logging to files.

# If we forget to specify path, this will make sure it is sane.
PATH=/usr/bin:/bin

get_program_stat() {
    stat --format="inode=%i  %n  length=%s   %y" "$0"
}

init_p27() {
    PROGRAM_STAT=$(get_program_stat)
}

quit_check() {
    program_stat=$(get_program_stat)
    if [ "X$PROGRAM_STAT" != "X$program_stat" ]; then
        echo "Program has changed.  Time to quit and be restarted."
        echo "  was: $PROGRAM_STAT"
        echo "  now: $program_stat"
        exit 0
    fi
}

log_line() {
    echo "$(date -Iseconds -u)  $*";
}

log_to_file() {
    init_p27
    # The full pathname to the log file.
    #
    # If I used something like /var/log/p27/ or /var/log/jeff/ I'd get
    # persistence but would also have to manage log rotation, which
    # makes the sort of ad hoc usage I have in my personal life more
    # difficult.
    log_path="/tmp/$1"

    # Save file descriptors so they can be restored to whatever they were
    # before redirection or used themselves to output to whatever they
    # were before the following redirect.
    exec 3>&1 4>&2
    # Restore file descriptors for particular signals. Not generally
    # necessary since they should be restored when the sub-shell exits.
    trap 'exec 2>&4 1>&3' 0 1 2 3
    # Redirect stdout to $log_file, then redirect stderr to stdout.  Note
    # that the order is important when you want them going to the same
    # file:  stdout must be redirected before stderr is redirected to
    # stdout.
    exec 1>>"$log_path" 2>&1
    # Everything below will go to {{ '{{' }} log_path {{ '}}' }}.

    # And also note that we're starting.
    for n in {1..4}; do echo; done
    echo "======================================================================"
    log_line "Starting"
    echo
}

