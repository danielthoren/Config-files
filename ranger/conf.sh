#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope ranger "$@"

dir=~/.config/ranger

print_green "Configuring ranger in folder $dir"

install_all $workingDir/deps.txt

if [ ! -d $dir ]; then
    echo "  Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "  Folder exists, purging existing data"
    rm "${dir}/rc.conf"
fi

ln -s "$workingDir/ranger_files/rc.conf" "${dir}"

echo "  Done configuring ranger"
echo ""
