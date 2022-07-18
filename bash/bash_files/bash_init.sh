#!/bin/bash


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
