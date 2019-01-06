#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)
path=`readlink -f $1`
declare -a files=($path/*.jpg)
files+=($path/*.jpeg)
files+=($path/*.png)
SIZE=10

i=1
d=1
mkdir "$path/$d"
for f in "${files[@]}"; do
	# Sort input files
	 mv "$f" "$path/$d"

	# Iterate counts
	((i++))
	((i_mod=$i % $SIZE))
	if [ "$i_mod" == 0 ]; then
		((d++))
		mkdir "$path/$d"
	fi
done
