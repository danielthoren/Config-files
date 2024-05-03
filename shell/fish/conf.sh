#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source $workingDir/../../functions.sh
source $workingDir/../../commandParser.sh -scope fish "$@"

print_green "  -> Configuring fish in folder $dir"

install_all $workingDir/deps.txt

fpath=$(which fish)
initFname='fish_init.fish'
wslInitFname='wsl_win10_display_init.fish'
installDir="$HOME/.config/fish"
targetInstallFile="$installDir/config.fish"

if flag_exists no-sudo ; then
    chsh -s $fpath
else
    sudo chsh -s $fpath $USER
    #    sudo usermod --shell $fpath $USER
fi

echo "    Set fish as default shell"

if [ ! -d $installDir ]; then
    echo "    Folder does not exist, creating folder"
    mkdir -p $installDir
else
    echo "    Folder exists, purging existing data"
    rm "${installDir}/$initFname"
    rm -r "${installDir}/functions"
fi

ln -s "$workingDir/fish_files/functions" "${installDir}"
ln -s "$workingDir/fish_files/$initFname" "${installDir}"

if ! grep -q $initFname $targetInstallFile; then
    echo "    Inserting source of $initFname into $targetInstallFile"
    echo "source $installDir/${initFname}" >> $targetInstallFile
fi

if ! grep -q $wslInitFname $targetInstallFile; then
    if yes_no_question "    Do you want to install win10 WSL display settings " ; then
        echo "    Inserting source of $wslInitFname into .bashrc"
        echo "source $installDir/functions/${wslInitFname}" >> $targetInstallFile
    fi
fi

echo "    Done configuring fish"
echo ""
