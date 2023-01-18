#!/bin/fish

################################################################################
#                              Init WSL settings                               #
################################################################################

#If in WSL, change display settings to use windows x-server
#and add alias for windows home

if grep -qi microsoft /proc/version;

   set -gx DISPLAY (route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0
   set -gx LIBGL_ALWAYS_INDIRECT 1

   set WIN_HOME_RAW (cmd.exe /c "<nul set /p=%UserProfile%" 2>/dev/null)
   set -gx WIN_HOME (wslpath $WIN_HOME_RAW)

   alias wslh="cd '$WIN_HOME'"
   alias wslg="cd '$WIN_HOME/git'"

    # Source fix for slow git in WSL2, will use git.exe if in dos system
    # alias git $FISH_FUNC_DIR/wsl_git_wrapper_runner.sh $argv
    # alias lgit /usr/bin/git $argv

end
