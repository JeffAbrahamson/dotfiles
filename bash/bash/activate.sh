#!/bin/bash

# Activate a python virtualenv.
# Create it if necessary.
# Populate it if necessary.

if [ -d venv ]; then
    . venv/bin/activate
else
    read -p "No virtual environment exists.  Create one?  " yesno
    case "$yesno" in
	yes|y|YES|Y )
	    virtualenv --python=python3 venv
	    . venv/bin/activate
	    if [ -r requirements.txt ]; then
		echo
		read -p "A requirements file exists.  Install requirements?  " require
		case "$require" in
		    yes|y|YES|Y )
			pip install -r requirements.txt
			;;
		    * )
			;;
		esac
	    fi
	    ;;
	* )
	    ;;
    esac
fi

