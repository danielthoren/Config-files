#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope fish "$@"

echo "Installing xmodmap bindings"

if ! command_exists xmodmap ; then
    echo "xmodmap not present on system, aborting..."
    exit -1
fi

ln -s "$workingDir/Xmodmap" "$HOME/.Xmodmap"

echo "Done configuring xmodmap bindings"
