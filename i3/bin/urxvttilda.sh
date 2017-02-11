#!/usr/bin/env zsh
NEXT_WAIT_TIME=0
COMMAND_STATUS=0
until [[ $COMMAND_STATUS  -eq 0 || $NEXT_WAIT_TIME -eq 4 ]]; do
    ping -c 1 -W 1 wttr.in
    COMMAND_STATUS=$?
    sleep $NEXT_WAIT_TIME
    let NEXT_WAIT_TIME=NEXT_WAIT_TIME+1
done
unset NEXT_WAIT_TIME
unset COMMAND_STATUS

curl wttr.in/london

echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
exec zsh
