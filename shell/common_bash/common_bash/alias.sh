#!/bin/bash -f

alias c++17="g++ -Wall -Wextra -Weffc++ -Wold-style-cast -Woverloaded-virtual -std=c++17 -pedantic"

alias pullall="$COMMON_BASH_DIR/pull_all_git_repo.sh"

alias eclean="find . -name '*~' -delete && find . -name '.#*' -delete"

alias logout="gnome-screensaver-command -l"

alias usbl="$COMMON_BASH_DIR/list_usb.sh"

alias lla="ls -la"

alias us="setxkbmap us -variant intl"
alias se="setxkbmap se"

# Start docker on WSL
alias startDocker="sudo /usr/sbin/service docker start"

alias sfconn="$COMMON_BASH_DIR/server_mount.sh"
alias sfdiss="$COMMON_BASH_DIR/server_diss.sh"
alias swconn="$COMMON_BASH_DIR/server_connect.sh"
