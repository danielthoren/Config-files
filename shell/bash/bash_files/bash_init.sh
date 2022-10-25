#!/bin/bash

export EDITOR='emacs'
export FIFUNC='/home/'$USER'/git/Config-files'
export COMMON_BASH_DIR="~/.config/common_bash"

alias c++17="g++ -Wall -Wextra -Weffc++ -Wold-style-cast -Woverloaded-virtual -std=c++17 -pedantic"

alias eclean="find . -name '*~' -delete && find . -name '.#*' -delete"

alias logout="gnome-screensaver-command -l"

alias usbl="$COMMON_BASH_DIR/list_usb.sh"

alias us="setxkbmap us -variant intl"
alias se="setxkbmap se"

alias startDocker="sudo /usr/sbin/service docker start"

alias sfconn="$COMMON_BASH_DIR/server_mount.sh"
alias sfdiss="$COMMON_BASH_DIR/server_diss.sh"
alias swconn="$COMMON_BASH_DIR/server_connect.sh"

################################################################################
#                              Init WSL settings                               #
################################################################################

#If in WSL, change display settings to use windows x-server
#and add alias for windows home
if grep -qi microsoft /proc/version; then
    export DISPLAY=$(route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0
    export LIBGL_ALWAYS_INDIRECT=1

    #wslHome=$(wslpath $(wslvar USERPROFILE))
    WIN_HOME_RAW="$(cmd.exe /c "<nul set /p=%UserProfile%" 2>/dev/null)"
    WIN_HOME="$(wslpath "$WIN_HOME_RAW")"

    alias wslh="cd '$WIN_HOME'"
    alias wslg="cd '$WIN_HOME/git'"
fi

################################################################################
#                               Fix keybindings                                #
################################################################################

#source remap_keys.sh
