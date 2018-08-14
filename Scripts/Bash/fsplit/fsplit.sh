#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)
path=`readlink -f $1`
declare -a FILES=($path/*.jpg)
FILES+=($path/*.jpeg)
FILES+=($path/*.png)

i=1
d=1
mkdir "$path/$d"
for f in "${FILES[@]}"; do
	# Sort input files
	 mv "$f" "$path/$d"

	# Iterate counts
	((i++))
	((i_mod=$i % 10))
	if [ "$i_mod" == 0 ]; then
		((d++))
		mkdir "$path/$d"
	fi
done
