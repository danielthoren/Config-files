#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/common_bash"

source $workingDir/../../functions.sh
source $workingDir/../../commandParser.sh -scope fish "$@"

if ! command_exists setxkbmap ; then
    if flag_exists no-sudo ; then
        echo "setxkbmap (x11-xkb-utils) not installed, cant install without sudo. Aliases 'us' & 'se' will not work..."
    else
        echo "setxkbmap (x11-xkb-utils) not installed, installing..."
        update
        install x11-xkb-utils
    fi
fi

echo "Configuring common bash in $dir"

if [ -d $dir ]; then
    echo "Folder exists, deleting..."
    rm -r "${dir}"
fi

ln -s "$workingDir/common_bash" $dir

echo "Done configuring common bash files in $dir"
