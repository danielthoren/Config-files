#!/bin/fish

#If in WSL, change display settings to use windows x-server

if ! grep -q WSL2 /proc/version
    exit 0
end

alias wslh="cd /mnt/c/Users/danth"
alias wslg="cd /mnt/c/Users/danth/git"


set DISPLAY route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'

set LIBGL_ALWAYS_INDIRECT 1
