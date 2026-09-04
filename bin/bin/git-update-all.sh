#!/bin/bash

## Assume that $cwd is a directory containing git repositories.
## Check each to see if it really does seem to be a git repository,
## then do an update and status.

# The arguments are a git command and its options.
git-do-this()
{
    for d in *; do
	(
	    if [ -d "$d" ]; then
		cd "$d" || exit
		if [ -d .git ]; then 
		    echo "==> $d <=="
		    git "$@"
		fi
	    fi
	);
    done
}

git-do-this remote update --prune
echo
echo ============================================================
echo
git-do-this pull
echo
echo ============================================================
echo
git-do-this status
