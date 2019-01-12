#!/bin/bash
# Workflow automation: edit inside child, move into parent, delete child

: ${1:?Directory parameter required}
printf "Cropdir:\t%s\n" "$1"
declare -a files="$1/*"
gimp ${files[@]}
mv ${files[@]} "$1/.."
rm -d $1
