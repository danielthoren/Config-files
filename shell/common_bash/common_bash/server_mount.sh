#!/bin/bash

#sudo sshfs -p 2022 daniel5908@158.174.68.120:/storage ~/server/storage

source $FIFUNC/functions.sh

source $FIFUNC/fish/fish_files/functions/read_server_config.sh

install net-tools

export HISTIGNORE='*sudo -S*'

home_mac="38:d5:47:7f:39:60"

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

sshfs -o password_stdin -p ${ssh_port} ${user}@${ip}:/storage ~/server/storage <<< $passw

sshfs -o password_stdin -p ${ssh_port} ${user}@${ip}:/srv/dev-disk-by-label-download ~/server/download <<< $passw
