#!/bin/fish

#If in WSL, change display settings to use windows x-server

if ! grep -q WSL2 /proc/version
    exit 0
end

alias wslh="cd /mnt/c/Users/danth"
alias wslg="cd /mnt/c/Users/danth/git"

exit 0