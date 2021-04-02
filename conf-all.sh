#!/bin/bash

#Vible flags are:
# -no-sudo  (for installation wighout sudo)
# -laptop   (for installation with laptop specific settings)

source functions.sh

update
$APT_UPGRADE

install htop
install git
install gparted
install net-tools
install sshpass

cd emacs
./conf.sh -all-conf "$@"
cd ../fish
./conf.sh -all-conf "$@"
cd ../kitty
.conf.sh -all-conf "$@"
cd ../ranger
./conf.sh -all-conf "$@"
