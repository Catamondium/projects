#!/bin/bash
# GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

> $1
# Misc codes
tput sgr0 >> $1 # Reset attributes
tput setaf 0 >> $1
echo "Foreground" >> $1

tput sgr0 >> $1 # Reset attributes
tput setab 255 >> $1
echo "Background" >> $1

tput sgr0 >> $1 # Reset attributes
tput rev >> $1
echo "Reverse video" >> $1

tput sgr0 >> $1 # Reset attributes
tput smul >> $1
echo "Underlined" >> $1
tput rmul >> $1

tput sgr0 >> $1 # Reset attributes
tput blink >> $1
echo "Blinking" >> $1

tput sgr0 >> $1 # Reset attributes
tput bold >> $1
echo "Bold" >> $1

tput sgr0 # Reset attributes
for C in {0..256}; do # Print colour codes
	tput sgr0 >> $1 # Reset attributes
	tput setab $C >> $1
	echo -ne "$C\t" >> $1
	if [ "$(($C % 16))" -eq 0 ]; then
		echo >> $1
	fi
done
tput sgr0 >> $1 # Reset attributes
echo >> $1 # Resetted newline
