#!/bin/sh
if bluetoothctl show | grep -q "Powered: yes"; then
    bluetoothctl power on
else
    bluetoothctl power off
fi
