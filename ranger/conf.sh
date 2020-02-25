#!/bin/bash

source ../functions.sh

dir=~/.config/ranger/

if ! command_exists ranger ; then
    echo "ranger not installed, installing..."
    sudo apt install ranger
fi

echo "Configuring ranger in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging existing data"
    rm "${dir}/rc.conf"
fi

ln -s "${PWD}/ranger_files/rc.conf" "${dir}"

echo "Done configuring ranger"
