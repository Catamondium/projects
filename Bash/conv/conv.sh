#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

subject="./data.txt"
result="./result.tsv"

chunk_width=3 # 3 newlines for every row of data
length=`wc -l < $subject` # Get number of newlines
let "length--" # Offset for final newline
let num=length/chunk_width

# Create .tsv
echo -e "name\tnum\tletter" > $result # Write header
for i in `seq 1 $chunk_width $length`; do # Read
	let "v=$i"
	name=`sed "${v}q;d" $subject` # Every 1st line
	let "v=$i+1"
	num=`sed "${v}q;d" $subject` # Every 2nd line
	let "v=$i+2"
	letter=`sed "${v}q;d" $subject` # Every 3rd line
	echo -e "$name\t$num\t$letter" >> $result #Write body row
done

