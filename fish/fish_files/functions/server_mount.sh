#!/bin/bash

#Viable flags are:
# -l (local connection)

source ../../../functions.sh
source ../../../commandParser.sh "$@"

export HISTIGNORE='*sudo -S*'

global_ip="155.4.155.164"
local_ip="192.168.1.100"

sshPort=2022
user=daniel5908

if flag_exists l ; then
    echo "Using local ip..."
    ip=$local_ip
else
    echo "Using global ip..."
    ip=$global_ip
fi

echo "Enter password:"
read -s password

echo "Connecting using ip ${ip}..."
    
echo $password | sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/srv/dev-disk-by-label-storage /mnt/server <<< $password

echo $password | sudo -S sshfs -o password_stdin -o allow_other -p ${sshPort} ${user}@${ip}:/srv/dev-disk-by-label-download /mnt/download <<< $password
