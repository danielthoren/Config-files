#!/bin/bash

dir=~/.config/fish

source ../functions.sh

source ../commandParser.sh -scope fish "$@"

if ! command_exists fish ; then
    if flag_exists no-sudo ; then
	echo "fish not installed, cant install without sudo, exiting..."
	exit no-sudo
    fi
    
    echo "fish not installed, installing..."
    install fish
fi

fpath=$(which fish)

echo "output: $fpath"
if flag_exists no-sudo ; then
    chsh -s $fpath
else    
    sudo chsh -s $fpath $USER
#    sudo usermod --shell $fpath $USER
fi

echo "Set fish as default shell"

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
