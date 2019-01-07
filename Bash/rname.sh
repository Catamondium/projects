#!/bin/bash
: '
Batch renamer following DIR/#.ext pattern,
where # is integer zfilled to width log10 of the number of files to mv
not recursing directories
'

verbose=false
while getopts "vh" c; do
	case "${c}" in
		v)
			verbose=true
			;;
		h | *)
			echo "Usage: $0 -[vh] [target]"
			echo "Options: -v verbose -h show this usage"
			exit 1
			;;
	esac
done

# suppressed readlink warnings
path=$(readlink -f $1 2> /dev/null)

: ${path:=$(pwd)} # default to current working dir

declare -a files=($path/*)

if [ "${#files[@]}" == 0 ]; then
	exit 0
fi

((width=$(log ${#files[@]}) + 1))

echo "About to rename inside:" $path
read -p "Are you sure? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	i=0
	for f in "${files[@]}"; do
		if [[ -f $f ]]; then
			ext=${f#*.}
			dest=$(printf "%s/%0*d.%s" "$path" "$width" "$i" "$ext")
			if $verbose; then
				echo $f "->" $dest
			fi
			mv "$f" "$dest" 2> /dev/null # Suppress 'x to x' error
			((i++))
		fi
	done
fi
