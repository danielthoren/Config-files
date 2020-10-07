#!/bin/bash

source $FIFUNC/functions.sh

install net-tools

install htop

export HISTIGNORE='*sudo -S*'

global_ip="155.4.147.233"
local_ip="192.168.1.100"

sshPort=2022
user=daniel5908

home_mac="38:d5:47:7f:39:60"

./server_diss.sh

if arp 192.168.1.1 | grep $home_mac; then
    echo "Using local ip..."
    ip=$local_ip
else
    echo "Using global ip..."
    ip=$global_ip
fi

echo "Enter password:"
read -s password

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

if ! [ -d ~/labbass ]; then
	echo "Dir '~/labbass' does not exist, creating dir..."
	mkdir ~/labbass
fi

echo "Connecting using ip ${ip}..."
    
echo $password | sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/storage ~/server/storage <<< $password

echo $password | sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/srv/dev-disk-by-label-download ~/server/download <<< $password

echo $password | sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/storage/Daniel/Documents/labass ~/labbass <<< $password
