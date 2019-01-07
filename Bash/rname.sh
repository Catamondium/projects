#!/bin/bash
: '
Batch renamer following DIR-#.ext pattern,
where # is integer zfilled to width log10 of the number of files to mv
not recursing directories
'

function rename() { # (path, recurse, verbose)
	declare -a files=($1/*)
	if [ "${#files[@]}" == 0 ]; then
		exit 0
	fi
	local width
	((width=$(log ${#files[@]}) + 1))
	local i=0
	for f in "${files[@]}"; do
		if [[ -f "$f" ]]; then
			local pname=${1##/*/}
			local ext=${f#*.}
			local dest=$(printf "%s/%s-%0*d.%s" "$1" "$pname" "$width" "$i" "$ext")
			if $3; then
				echo $f "->" $dest
			fi
			mv "$f" "$dest" 2> /dev/null # Suppress 'x to x' error
			((i++))
		elif [[ $2 && -d "$f" ]]; then
			rename "$f" $2 $3
		fi
	done
}

function verify() { # (path)
echo "About to rename inside:" ${1##/*/}
read -p "Are you sure? " -n 1 -r
echo
return $([[ $REPLY =~ ^[Yy]$ ]])
}

verbose=false
force=false
recurse=false
while getopts "vhfir" c; do
	case "${c}" in
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
			printf "Usage: $0 -[vhfir] [target]\n"
			printf "Options:\n\t-v Verbose\n"
			printf "\t-h Show this usage\n"
			printf "\t-r Rename recursively\n"
			printf "\t-f Don't ask for approval\n"
			printf "\t-i Require approval\n"
			exit 1
			;;
	esac
done

# suppressed readlink warnings
path=$(readlink -f $1 2> /dev/null)

: ${path:=$(pwd)} # default to current working dir

if $force || verify $path; then
	rename $path $recurse $verbose
fi
