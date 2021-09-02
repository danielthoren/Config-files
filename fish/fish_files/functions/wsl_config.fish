#!/bin/bash

#If in WSL, change display settings to use windows x-server

if grep -qi Microsoft /proc/version
    set DISPLAY (ip route | awk '/^default/{print $3; exit}'):0.0
end
