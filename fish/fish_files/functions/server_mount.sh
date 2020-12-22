#!/bin/bash

source $FIFUNC/functions.sh

install net-tools
install htop

export HISTIGNORE='*sudo -S*'

global_ip="158.174.68.120"
local_ip="192.168.1.100"

sshPort=2022
user=daniel5908

PASSW_FILE=~/.config/fish/server_passw

if [ -e "$PASSW_FILE" ]; then
    read PASSW < $PASSW_FILE
else
    echo "No $PASSW_FILE file detected, aborting..."
    exit -1
fi

home_mac="e0:b9:e5:e1:1b:7e"

if arp 192.168.1.1 | grep $home_mac; then
    echo "Using local ip..."
    ip=$local_ip
else
    echo "Using global ip..."
    ip=$global_ip
fi

#sudo -S bash $FIFUNC/fish/fish_files/functions/server_diss.sh
bash $FIFUNC/fish/fish_files/functions/server_diss.sh

if ! [ -d ~/server ]; then
	echo "Dir '~/server' does not exist, creating dir..."
	mkdir ~/server
fi

if ! [ -d ~/server/download ]; then
	echo "Dir '~/server/download' does not exist, creating dir..."
	mkdir ~/server/download
fi

if ! [ -d ~/server/storage ]; then
	echo "Dir '~/server/storage' does not exist, creating dir..."
	mkdir ~/server/storage
fi

echo "Connecting using ip ${ip}..."
    
sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/storage ~/server/storage <<< $PASSW

sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/srv/dev-disk-by-label-download ~/server/download <<< $PASSW
