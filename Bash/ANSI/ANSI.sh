#!/bin/bash
# GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

echo -e "prefix_escape\tlabel/colour\tpostfix_escape" > $1
# Misc codes
tput setaf 0 >> $1
echo -e "\tForeground\t" >> $1

tput setab 255 >> $1
echo -e "\tBackground\t" >> $1

tput rev >> $1
echo -e "\tReverse video\t" >> $1


tput smul >> $1
echo -ne "\tUnderlined\t" >> $1
tput rmul >> $1
echo >> $1

tput blink >> $1
echo -e "\tBlinking\t" >> $1


tput bold >> $1
echo -e "\tBold\t" >> $1

tput sgr0 >> $1 # Reset attributes
echo -e "\tReset\t" >> $1
for C in {0..255}; do # Print colour codes
	tput setab $C >> $1
	echo -e "\t$C\t" >> $1
done

tput sgr0 # Reset attributes
echo >> $1 # Final newline
