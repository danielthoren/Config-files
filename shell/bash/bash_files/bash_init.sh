#!/bin/bash

export EDITOR='emacs'
export FIFUNC=$HOME/git/Config-files
export COMMON_BASH_DIR=$HOME/.config/common_bash

# Source common alias betwee fish and bash
source $COMMON_BASH_DIR/alias.sh
source $FIFUNC/functions.sh

################################################################################
#                              Init WSL settings                               #
################################################################################

#If in WSL, change display settings to use windows x-server
#and add alias for windows home
if in_wsl; then
    export DISPLAY=$(route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0
    export LIBGL_ALWAYS_INDIRECT=1

    #wslHome=$(wslpath $(wslvar USERPROFILE))
    WIN_HOME_RAW="$(cmd.exe /c "<nul set /p=%UserProfile%" 2>/dev/null)"
    WIN_HOME="$(wslpath "$WIN_HOME_RAW")"

    alias wslh="cd '$WIN_HOME'"
    alias wslg="cd '$WIN_HOME/git'"

    # Source fix for slow git in WSL2, will use git.exe if in dos system
    # source $COMMON_BASH_DIR/wsl_git_wrapper.sh
fi

################################################################################
#                               Fix keybindings                                #
################################################################################
