#!/bin/bash

dir=~/.emacs.d

source ../functions.sh

if ! command_exists emacs ; then
    echo "emacs not installed, installing..."
    #echo "Adding repository to sources list..."
    #sudo add-apt-repository ppa:kelleyk/emacs
    #sudo apt update
    sudo apt install -y emacs
fi

#Regex program used for dumb-jump-mode
if ! command_exists ag ; then
    echo "ag not installed, installing..."
    sudo apt install -y silversearcher-ag
fi

if ! command_exists ditaa ; then
    echo "ditaa not installed, installing..."
    sudo apt-get install -y ditaa
fi

#used by jedi (python autocomplete)
if ! command_exists virtualenv ; then
    echo "virtualenv not installed, installing..."
    sudo apt install -y virtualenv
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
