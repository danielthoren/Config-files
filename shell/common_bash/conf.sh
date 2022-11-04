#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/common_bash"

echo "Configuring common bash in $dir"

if [ -d $dir ]; then
    echo "Folder exists, deleting..."
    rm -r "${dir}"
fi

ln -s "$workingDir/common_bash" $dir

echo "Done configuring common bash files in $dir"
