#!/bin/bash

dir=~/.config/kitty

source ../functions.sh

source ../commandParser.sh -scope kitty "$@"

if ! command_exists kitty ; then
    if flags_exists no-sudo ; then
	echo "kitty not installed, cant install without sudo, exiting..."
	exit no-sudo
    fi
    
    echo "kitty not installed, installing..."
    sudo apt update
    $APT_INSTALL kitty
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

ln -s "${PWD}/kitty_files/kitty.conf" "${dir}"
ln -s "${PWD}/kitty_files/kitty-themes" "${dir}"

echo "Done configuring kitty"
