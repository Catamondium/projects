#!/bin/bash

function sTime() {
	echo $(printf "%02d:%02d" $1 $2)
}

function elapse() {
	# calculate total minutes
	local offset=$(($1 * 60 + $2))
	local tot=$((offset + $3))
	# form hh:mm
	hrs=$((tot / 60))
	hrs=${hrs%.*} # floor
	mins=$((tot % 60))
	echo $(sTime $hrs $mins)
}

function usage() {
	printf "Usage: $0 [-vh] hh:mm mins_elapse\n"
	printf "Options:\n\t-q quietly output end time\n"
	printf "\t-h print this message and exit\n"
	exit 1
}

quiet=false
while getopts "qh" c; do
	case "${c}" in
		q)
			quiet=true
			;;
		h | *)
			usage
			;;
		esac
	done
shift $((OPTIND-1))

if [ $# -lt 2 ]; then
	usage
fi

s_hrs=${1%:*} # Delete from ':' onwards
s_mins=${1#*:} # Delete from ':' backwards
e_time=$(elapse $s_hrs $s_mins $2)

if ! $quiet ; then
	s_time=$(sTime $s_hrs $s_mins)
	printf "Start:\t%s\t%+d\nEnd:\t%s\n" $s_time $2 $e_time
else
	echo $e_time
fi
