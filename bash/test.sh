#!/bin/bash

test_file()
{
    echo Testing $1...
    #bash --login --norc $1
    bash $1
    if [ $? != 0 ]; then
	echo Failed on checking $1
	ok=1
    fi
}

ok=0
test_file bashrc
test_file bash_profile
test_file bash_logout

exit $ok
