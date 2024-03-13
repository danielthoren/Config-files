#!/bin/bash

#Vible flags are:
# -no-sudo  (for installation wighout sudo)
# -laptop   (for installation with laptop specific settings)

source functions.sh

workingDir=$(dirname -- "$( readlink -f -- "$0"; )";)

print_green "Configuring"

update
upgrade

install_all $workingDir/deps.txt

bash $workingDir/emacs/conf.sh -all-conf "$@"
bash $workingDir/shell/conf.sh -all-conf "$@"
bash $workingDir/gdb/conf.sh -all-conf "$@"
bash $workingDir/xmodmap/conf.sh -all-conf "$@"

#$workingDir/kitty/conf.sh -all-conf "$@"
#$workingDir/ranger/conf.sh -all-conf "$@"
