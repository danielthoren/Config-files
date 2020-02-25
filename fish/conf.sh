#!/bin/bash

dir=~/.config/fish/

source ../functions.sh

if ! command_exists fish ; then
    echo "fish not installed, installing..."
    sudo apt install fish
    fpath= which fish
    echo $fpath | sudo tee -a /etc/shells
    chsh -s $fpath
    echo "Set fish as default shell"
fi

echo "Configuring fish in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging existing data"
    rm "${dir}/config.fish"
    rm -r "${dir}/functions"
fi

ln -s "${PWD}/fish_files/functions" "${dir}"
ln -s "${PWD}/fish_files/config.fish" "${dir}"

echo "Done configuring fish"
