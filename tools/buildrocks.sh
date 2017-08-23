#!/bin/bash

LINKAGE=$1
if [ -z $LINKAGE ]; then
	LINKAGE="shared_lib"
fi

JOBS=$2
if [ -z $JOBS ]; then
	JOBS=4
fi

run ()
{
	COMMAND=$1
	# check for empty commands
	if test -z "$COMMAND" ; then
		echo -e "\033[1;5;31mERROR\033[0m No command specified!"
		return 1
	fi

	shift;
	OPTIONS="$@"
	# print a message
	if test -n "$OPTIONS" ; then
		echo -ne "\033[1m$COMMAND $OPTIONS\033[0m ... "
	else
		echo -ne "\033[1m$COMMAND\033[0m ... "
	fi

	# run or die
	$COMMAND $OPTIONS ; RESULT=$?
	if test $RESULT -ne 0 ; then
		echo -e "\033[1;5;31mERROR\033[0m $COMMAND failed. (exit code = $RESULT)"
		exit 1
	fi

	echo -e "\033[0;32myes\033[0m"
	return 0
}


echo "*** Building RocksDB... "

USERDIR=$PWD            # Save current dir and return to it later

run git submodule update --init rocksdb

run cd rocksdb
run git checkout v5.5.3
CFLAGS=-fPIC run make -j$JOBS $LINKAGE
run cd $USERDIR         # Return to user's original directory
