#!/usr/bin/env bash
exec notify-send -i "$(bluetoothctl info | grep Icon | awk '{print $2}')" "$(bluetoothctl info | grep Name | awk '{print $2}')"
