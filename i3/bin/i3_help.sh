#!/usr/bin/env bash

DURATION=5000
ICON=/usr/share/icons/Numix/48/actions/help.svg

notify-send -t $DURATION -i $ICON "${1}" "${2}"
