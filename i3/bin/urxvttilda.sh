#!/usr/bin/env zsh
NEXT_WAIT_TIME=1
COMMAND_STATUS=-1
until [[ $COMMAND_STATUS -eq 0 || $NEXT_WAIT_TIME -eq 6 ]]; do
    curl wttr.in/london
    COMMAND_STATUS=$?
    sleep $NEXT_WAIT_TIME
    let NEXT_WAIT_TIME=NEXT_WAIT_TIME+1
done
unset NEXT_WAIT_TIME
unset COMMAND_STATUS


echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
exec zsh
