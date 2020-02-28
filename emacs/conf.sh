#!/bin/bash

dir=~/.emacs.d/

source ../functions.sh

if ! command_exists emacs26 ; then
    echo "emacs26 not installed, installing..."
    sudo apt install emacs26
fi

#Regex program used for dumb-jump-mode
if ! command_exists ag ; then
    echo "ag not installed, installing..."
    sudo apt install silversearcher-ag
fi

echo "Configuring emacs in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging data"
    rm "${dir}/init.el"
    rm -r "${dir}/funs"
    rm -r "${dir}/settings"
fi

ln -s "${PWD}/emacs_files/settings" "${dir}"
ln -s "${PWD}/emacs_files/funs" "${dir}"
ln -s "${PWD}/emacs_files/init.el" "${dir}"

echo "Done configuring emacs"
