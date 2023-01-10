#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=~/.config/kitty

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope kitty "$@"

if ! command_exists kitty ; then
    if flags_exists no-sudo ; then
        echo "kitty not installed, cant install without sudo, exiting..."
        exit no-sudo
    fi

    echo "kitty not installed, installing..."
    update
    install kitty
fi

echo "installing kitty in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging data"
    rm "${dir}/kitty.conf"
    rm -r "${dir}/kitty-themes"
fi

ln -s "$workingDir/kitty_files/kitty.conf" "${dir}"
ln -s "$workingDir/kitty_files/kitty-themes" "${dir}"

echo "Done configuring kitty"
