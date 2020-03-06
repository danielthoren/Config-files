#!/bin/bash

dir=~/.config/kitty

source ../functions.sh

if ! command_exists kitty ; then
    echo "kitty not installed, installing..."
    sudo apt update
    sudo apt install kitty
fi

echo "kity emacs in folder $dir"

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
