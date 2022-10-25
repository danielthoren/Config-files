#!/bin/bash

#Vible flags are:
# -no-sudo  (for installation wighout sudo)
# -laptop   (for installation with laptop specific settings)

source functions.sh

workingDir=$(dirname -- "$( readlink -f -- "$0"; )";)

update
$APT_UPGRADE

install htop
install git
install gparted
install net-tools
install sshpass

$workingDir/emacs/conf.sh -all-conf "$@"
$workingDir/shell/fish/conf.sh -all-conf "$@"
$workingDir/kitty/conf.sh -all-conf "$@"
$workingDir/ranger/conf.sh -all-conf "$@"
