#!/bin/bash

## Assume that $cwd is a directory containing git repositories.
## Check each to see if it really does seem to be a git repository,
## then do an update and status.

# $1 is a git action
git-do-this()
{
    for d in *; do
	(
	    if [ -d $d ]; then 
		cd $d; 
		if [ -d .git ]; then 
		    echo '==> ' $d ' <=='
		    git $1; 
		fi
	    fi
	);
    done
}

git-do-this pull
echo
echo ============================================================
echo
git-do-this status