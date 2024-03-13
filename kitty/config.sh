#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=~/.config/kitty

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope kitty "$@"

print_green "Configuring kitty in folder $dir"

install_all $workingDir/deps.txt

if [ ! -d $dir ]; then
    echo "  Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "  Folder exists, purging data"
    rm "${dir}/kitty.conf"
    rm -r "${dir}/kitty-themes"
fi

ln -s "$workingDir/kitty_files/kitty.conf" "${dir}"
ln -s "$workingDir/kitty_files/kitty-themes" "${dir}"

echo "  Done configuring kitty"
echo ""
