#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/fish"

source $workingDir/../../functions.sh
source $workingDir/../../commandParser.sh -scope fish "$@"

print_green "  -> Configuring fish in folder $dir"

install_all $workingDir/deps.txt

fpath=$(which fish)

if flag_exists no-sudo ; then
    chsh -s $fpath
else
    sudo chsh -s $fpath $USER
    #    sudo usermod --shell $fpath $USER
fi

echo "    Set fish as default shell"

if [ ! -d $dir ]; then
    echo "    Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "    Folder exists, purging existing data"
    rm "${dir}/config.fish"
    rm -r "${dir}/functions"
fi

ln -s "$workingDir/fish_files/functions" "${dir}"
ln -s "$workingDir/fish_files/config.fish" "${dir}"

echo "    Done configuring fish"
echo ""
