#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)
path=`readlink -f $1`
declare -a files=($path/*.jpg)
files+=($path/*.jpeg)
files+=($path/*.png)
SIZE=10

if [ "${#files[@]}" == 0 ]; then
	exit 0
fi

i=1
d=1
((width=$(log ${#files[@]}) + 1))
dir=$(printf "%s/%0*d\n" "$path" "$width" "$d")
mkdir "$dir"
for f in "${files[@]}"; do
	# Sort input files
	mv "$f" "$dir"

	# Iterate counts
	((i++))
	((i_mod=$i % $SIZE))
	if [ "$i_mod" == 0 ]; then
		((d++))
		dir=$(printf "%s/%0*d\n" "$path" "$width" "$d")
		echo $dir
		mkdir "$dir"
	fi
done
