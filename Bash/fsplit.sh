#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)
path=$(readlink -f $1)
declare -a files=($path/*.jpg)
files+=($path/*.jpeg)
files+=($path/*.png)
SIZE=10

if [ "${#files[@]}" == 0 ]; then
	exit 0
fi

((div=${#files[@]} / $SIZE))
div=${div%.*}
((width=$(log $div) + 1))

i=0
for f in "${files[@]}"; do
	((i_mod=$i % $SIZE))
	if [ "$i_mod" == 0 ]; then
		((d=$i / $SIZE))
		d=${d%.*}
		dir=$(printf "%s/%0*d" "$path" "$width" "$d")
		mkdir "$dir"

		echo $dir
	fi

	mv "$f" "$dir"
	((i++))
done
