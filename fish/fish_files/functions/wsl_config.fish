#!/bin/bash

#If in WSL, change display settings to use windows x-server

if grep -qi Microsoft /proc/version; then
    set disp (ip route | awk '/^default/{print $3; exit}'):0.0
    echo $disp
    set DISPLAY $disp
end
