#!/bin/fish

#If in WSL, change display settings to use windows x-server

if grep -q WSL2 /proc/version
   set DISPLAY (route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0
   set LIBGL_ALWAYS_INDIRECT 1
end