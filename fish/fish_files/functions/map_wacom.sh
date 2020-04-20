#!/bin/bash

for num in $(xinput list | grep Wacom | awk '{print $9}' | sed $num 's/id=//');
do
    echo $num
    xinput map-to-output $num DVI-D-0
done


# xinput map-to-output 12 DVI-D-0
# xinput map-to-output 10 DVI-D-0
# xinput map-to-output 11 DVI-D-0
# xinput map-to-output 15 DVI-D-0
# xinput map-to-output 16 DVI-D-0
  
