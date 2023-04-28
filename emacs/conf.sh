#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=~/.emacs.d

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh "$@"

if flag_exists no-sudo ; then
    echo exists
fi

if ! command_exists emacs ; then
    if flag_exists no-sudo ; then
        echo "emacs not installed, cant install without sudo, exiting..."
        exit no-sudo
    fi

    add_source ppa:kelleyk/emacs

    echo "emacs not installed, installing..."

    echo "Adding repository to sources list..."
    sudo add-apt-repository ppa:kelleyk/emacs

    install emacs28
fi

#installing ccls (c/c++ language server)
if flag_exists no-sudo ; then
    echo "cant install ccls without sudo..."
else
    echo "installing ccls..."
    install ccls
fi

if flag_exists no-sudo ; then
    echo "cant install irony-server without sudo..."
else
    echo "installing irony-server..."
    install irony-server
fi

#Regex program used for dumb-jump-mode
if ! command_exists ag ; then
    if flag_exists no-sudo ; then
        echo "ag not installed, cant install without sudo..."
    else
        echo "ag not installed, installing..."
        install silversearcher-ag
    fi
fi

if ! command_exists ditaa ; then
    if flag_exists no-sudo ; then
        echo "ditaa not installed, cant install without sudo..."
    else
        echo "ditaa not installed, installing..."
        install ditaa
    fi
fi

if ! command_exists pip3 ; then
    if command_exists no-sudo ; then
        echo "pip3 not installed, cant install without sudo..."
    else
        echo "pip3 not installed, installing..."
        install python3-pip
    fi
fi

#Python language server
if ! command_exists pyright ; then
    if command_exists no-sudo ; then
        echo "pyright not installed, cant install without sudo..."
    else
        echo "pyright not installed, installing..."
        sudo pip3 install pyright
    fi
fi

if ! command_exists virtualenv ; then
    if command_exists no-sudo ; then
        echo "virtualenv not installed, cant install without sudo..."
    else
        echo "virtualenv not installed, installing..."
        install virtualenv
    fi
fi

# Markdown parser
if ! command_exists virtualenv ; then
    if command_exists no-sudo ; then
        echo "markdown not installed, cant install without sudo..."
    else
        echo "markdown not installed, installing..."
        install markdown
    fi
fi

echo "Configuring emacs in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging data"
    rm "${dir}/init.el"
    rm -r "${dir}/functions"
    rm -r "${dir}/settings"
    rm -r "${dir}/gendoxy"
    rm -r "${dir}/Block-Comment-Mode"
fi

git submodule init
git submodule update

ln -s "$workingDir/emacs_files/settings" "${dir}"
ln -s "$workingDir/emacs_files/functions" "${dir}"
ln -s "$workingDir/emacs_files/gendoxy" "${dir}"
ln -s "$workingDir/emacs_files/Block-Comment-Mode" "${dir}"

ln -s "$workingDir/emacs_files/init.el" "${dir}"

echo "Done configuring emacs"
