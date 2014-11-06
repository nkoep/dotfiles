#!/bin/sh

set -e
set -o nounset

DELAY=$(zenity --entry --text "Enter delay...")
if [ "$DELAY" = "" ]; then
    exit 0
elif [[ ! "$DELAY" =~ ^[0-9]+$ ]]; then
    zenity --error --text "Invalid delay!"
    exit 1
else
    sudo shutdown -c
    sudo shutdown -h $DELAY
fi

