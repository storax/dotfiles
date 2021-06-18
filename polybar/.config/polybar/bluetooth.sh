#!/bin/sh
if bluetoothctl show | grep -q "Powered: yes"; then
    if bluetoothctl info | grep -q 'Device'; then
        echo "%{F#6ca0a3}"
    else
        echo "%{F#acd0d3}"
    fi
else
    echo "%{F#dcdccc}"
fi
