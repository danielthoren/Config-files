#!/bin/bash

DISP_FILE=~/.config/fish/main_display
echo $DISP_FILE

if [ -e "$DISP_FILE" ]; then
    read MAIN_DISPLAY < $DISP_FILE
    echo $MAIN_DISPLAY
else
    echo "No $DISP_FILE file detected, aborting..."
    exit -1
fi

for num in $(xinput list | grep Wacom | awk '{print $9}' | sed $num 's/id=//');
do
    echo $num
    xinput map-to-output $num $MAIN_DISPLAY
done
  
