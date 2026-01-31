#!/bin/bash

set -euo pipefail

export SSH_USER=$LOGNAME

action="$1"
shift || true
case "$action" in
    test)
	docker compose -f compose.yml up --build dev-test
	;;
    sh)
	docker compose up -d --build dev-sh
	docker compose exec dev-sh /usr/local/bin/entrypoint.sh /bin/bash

	echo "Checking if other shells are running..."
	num_shells=$(docker compose exec dev-sh ps -eo tty,comm | awk '$1 ~ /^pts\// && $2=="bash" {print $1}' | sort -u | wc -l)
	if [ "$num_shells" -ne 0 ]; then
	    echo "${num_shells} still running."
	else
	    echo "No more shells detected, stopping container..."
	    docker compose stop dev-sh
	fi
	;;
    claude)
	# The following fussing about is because /usr/local/bin/claude
	# is an interpetted file (starts with #!), which is somehow
	# leading to HOME being set to the real value for the uid
	# invoked by gosu in entrypoint.sh.
	#
	# The hacky workaround here is that entrypoint.sh explicitly
	# detects that we want to run claude, then runs directly the
	# node file to which /usr/local/bin/claude points.
	#
	# This is obviously fragile to something about the claude
	# installation changing.  If that happens, we should change to
	# running bash, check what's being executed, and then execute
	# that instead.  It's also possibel that future iterations of
	# something will obviate the need for this hack.

	# docker compose run --rm --build dev-claude /usr/local/bin/claude
	docker compose run --rm --build dev-claude claude
	# docker compose run --rm --build dev-claude /bin/bash
	;;
    codex)
	docker compose run --rm --build dev-codex codex
	;;

    *)
	echo "Usage: ./docker-manage.sh test|sh|claude|codex"
	echo "** Unrecognised action: \"$action\"."
        ;;
esac
