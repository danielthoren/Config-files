#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/common_bash"

source $workingDir/../../functions.sh
source $workingDir/../../commandParser.sh -scope fish "$@"

print_green "  -> Configuring common bash in $dir"

install_all $workingDir/deps.txt

if [ -d $dir ]; then
    echo "    Folder exists, deleting..."
    rm -r "${dir}"
fi

ln -s "$workingDir/common_bash" $dir

echo "    Done configuring common bash files in $dir"
echo ""
