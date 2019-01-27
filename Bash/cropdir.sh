#!/bin/bash
# Workflow automation: edit inside child, move into parent, delete child

function verify()
{ 
	read -p "Are you sure? " -n 1 -r
	echo
	return $([[ $REPLY =~ ^[Yy]$ ]])
}

function usage()
{
	printf "Usage:\t$0 -[hfi] [targets]\n"
	printf "[targets] defaults to all directories in working dir\n"
	printf "Options:\n"
	printf "\t-i Ask between directories\n"
	printf "\t-f Don't ask between directories\n"
	exit 1
}

force=true
while getopts "hif" c; do
	case "${c}" in
		i)
			force=false
			;;
		f)
			force=true
			;;
		h | *)
			usage
			;;
	esac
done

shift $((OPTIND-1))
declare -a args=$@
: ${args:="$PWD/*"}

for f in ${args[@]}; do
	if [ -d $f ]; then
		printf "Cropdir:\t%s\n" "$f"
		if $force || verify; then
			declare -a files="$f/*"
			gimp ${files[@]}
			mv ${files[@]} "$f/.."
			rm -d $f
		fi
	fi
done
