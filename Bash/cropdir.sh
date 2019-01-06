#!/bin/bash
# Workflow automation, crop inside child, move into parent, delete child

printf "Cropdir:\t%s\n" "$1"

files="$1/*"
gimp $files
mv $files ".."
rm -di $1
