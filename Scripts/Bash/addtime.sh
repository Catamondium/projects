#!/bin/bash

function elapse() {
	local hrs=${1%:*} # delete from ':' onwards
	local mins=${1#*:} # delete from ':' backwards
	# calculate mins to add
	hrs=$(($hrs * 60))
	local tot=$((hrs + mins + $2))
	# form hh:mm
	hrs=$((tot / 60))
	hrs=${hrs%.*}
	mins=$((tot % 60))
	echo $(printf "%02d:%02d" $hrs $mins)
}

if [ $# -lt 2 ]; then
	printf "Usage: %s hh:mm mins_elapse\n" $0
	exit 1
fi

elapse $1 $2
