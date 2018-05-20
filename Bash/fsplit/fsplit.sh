#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

path=`readlink -f $1`
FILES=($path/*.jpg)

i=1
d=1
mkdir "$1/$d"
for f in "${FILES[@]}"; do
	# Sort input files
	let "i_mod=$i % 5"
	 mv "$f" "$1/$d"

	# Iterate counts
	let "i++"
	if [ "$i_mod" == 0 ]; then
		let "d++"
		mkdir "$1/$d"
	fi
done
