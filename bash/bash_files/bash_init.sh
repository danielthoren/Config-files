#!/bin/bash


################################################################################
#                              Init WSL settings                               #
################################################################################

#If in WSL, change display settings to use windows x-server
#and add alias for windows home
if grep -q WSL2 /proc/version; then
    export DISPLAY=$(route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0
    export LIBGL_ALWAYS_INDIRECT=1

    wslHome=$(wslpath $(wslvar USERPROFILE))

    alias wslh="cd $wslHome"
    alias wslg="cd $wslHome/git"
fi

################################################################################
#                               Fix keybindings                                #
################################################################################

source remap_keys.sh
