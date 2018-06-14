#!/bin/bash
# GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

subject="./data.txt"
result="./result.tsv"

chunk_width=3 # 3 lines for every row of data
length=`wc -l < $subject` # Get number of newlines
((length--)) # Offset for final newline

# Create .tsv
echo -e "name\tnum\tletter" > $result # Write header

for i in `seq 1 $chunk_width $length`; do # Read
	((v=$i))
	name=`sed "${v}q;d" $subject` # Every 1st line

	((v=$i+1))
	num=`sed "${v}q;d" $subject` # Every 2nd line

	((v=$i+2))
	letter=`sed "${v}q;d" $subject` # Every 3rd line

	echo -e "$name\t$num\t$letter" >> $result # Write body row
done

