#!/bin/bash

source ../functions.sh
source ../commandParser.sh -scope ranger "$@"

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

dir=~/.config/ranger

if ! command_exists ranger ; then
    if flags_exists no-sudo ; then
    echo "ranger not installed, cant install without sudo, exiting..."
    exit no-sudo
    fi

    echo "ranger not installed, installing..."
    install ranger
fi

echo "Configuring ranger in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging existing data"
    rm "${dir}/rc.conf"
fi

ln -s "$workingDir/ranger_files/rc.conf" "${dir}"

echo "Done configuring ranger"
