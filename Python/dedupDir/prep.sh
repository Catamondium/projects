#!/bin/bash
KEEP='./keepdir'
CHANGE='./dir'

if [ -d "$CHANGE" ]; then
    rm -drf $CHANGE
    cp -r $KEEP $CHANGE
fi