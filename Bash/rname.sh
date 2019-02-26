#!/bin/bash

function rename() # (path, recurse, dry, verbose)
{ 
	regex="^$1/${1##/*/}-\d+.*$"
	regex=${regex//\//\\/} # Escape contained '/'
	declare -a files=(reSort "$regex" $1/*)
	if [ "${#files[@]}" == 0 ]; then
		exit 0
	fi
	local width
	((width=$(log ${#files[@]}) + 1))
	local i=0
	for f in "${files[@]}"; do
		if [[ -f "$f" ]]; then
			local dest=$(printf "%s/%s-%0*d.%s" "$1" "${1##/*/}" "$width" "$i" "${f##*.}")
			if $4; then printf "\"%s\" -> \"%s\"\n" "$f" "$dest"; fi
			# Suppress 'x to x' error 
			if ! $3; then mv -n "$f" "$dest" 2> /dev/null; fi
			((i++))
		elif [[ $2 && -d "$f" ]]; then
			rename "$f" $2 $3 $4
		fi
	done
}

function verify() # (path)
{ 
	echo "About to rename inside:" ${1##/*/}
	read -p "Are you sure? " -n 1 -r
	echo
	return $([[ $REPLY =~ ^[Yy]$ ]])
}

function usage()
{
	printf "Batch renamer following DIR-#.ext pattern.\n"
	printf "# is an integer zfilled to width log10 of " 
	printf "the number of files to mv recursing directories\n"
	printf "Usage:\t$0 -[vhfir] [targets]\n"
	printf "Options:\n"
	printf "\t-d Dry run, verbosely\n"
	printf "\t-v Verbose\n"
	printf "\t-r Rename recursively\n"
	printf "\t-f Don't ask for approval\n"
	printf "\t-i Require approval\n"
	printf "\t-h Show this usage\n"
	exit 1
}

verbose=false
force=false
recurse=false
dry=false
while getopts "dvhfir" c; do
	case "${c}" in
		d)
			dry=true
			verbose=true
			;;
		v)
			verbose=true
			;;
		r)
			recurse=true
			;;
		f)
			force=true
			;;
		i)
			force=false
			;;
		h | *)
			usage
			;;
	esac
done

shift $((OPTIND-1))

declare -a args=$@
: ${args:=$(pwd)} # default to working dir

for f in ${args[@]}; do
	path=$(readlink -f $f 2> /dev/null) # suppressed warnings
	if [ -d $path ]; then
		if $dry || $force || verify $path; then
			rename $path $recurse $dry $verbose
		fi
	fi
done
