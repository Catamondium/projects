#!/bin/bash
# GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

echo -e "prefix_escape\ttput operation\tpostfix_escape" > $1 # Header

tput smul >> $1 # Underline exception
echo -ne "\tsmul; rmul\t" >> $1
tput rmul >> $1
echo >> $1

declare -a OPs=(rev blink bold sgr0 setaf setab)
for OP in "${OPs[@]}"; do
	if [ "$OP" == "setaf" ] || [ "$OP" == "setab" ]; then
		for C in {0..255}; do # Send colour codes
			tput $OP $C >> $1
			echo -e "\t$OP $C\t" >> $1
		done
	else # Send misc codes
		tput $OP >> $1
		echo -e "\t$OP\t" >> $1
	fi
done
echo >> $1 # Final newline

tput sgr0 # Reset attributes
#cat $1
