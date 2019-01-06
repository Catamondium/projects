#!/bin/bash
path=$(readlink -f $1)
size=$2

: ${size:=10}     # default bucket size 10
: ${path:=$(pwd)} # default to current working dir

declare -a files=($path/*.jpg)
files+=($path/*.jpeg)
files+=($path/*.png)

if [ "${#files[@]}" == 0 ]; then
	exit 0
fi

((div=${#files[@]} / $size))
div=${div%.*}
((width=$(log $div) + 1))

i=0
for f in "${files[@]}"; do
	((i_mod=$i % $size))
	if [ "$i_mod" == 0 ]; then
		((d=$i / $size))
		d=${d%.*}
		dir=$(printf "%s/%0*d" "$path" "$width" "$d")
		mkdir "$dir"

		echo $dir
	fi

	mv "$f" "$dir"
	((i++))
done
