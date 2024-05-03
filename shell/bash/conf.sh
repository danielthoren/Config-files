#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
installDir=$HOME

initFname='.bash_init.sh'
wslInitFname='.wsl_win10_display_init.sh'

source $workingDir/../../functions.sh

print_green "  -> Configuring bash in $installDir"

if [ -f ${installDir}/${initFname} ]; then
    echo "    Bash config link exists, removing..."
    rm "${installDir}/${initFname}"
fi

ln -s "$workingDir/bash_files/bash_init.sh" "${installDir}/${initFname}"

if ! grep -q $initFname  ~/.bashrc; then
    echo "    Inserting source of .bash_init.sh into .bashrc"
    echo "source $installDir/${initFname}" >> ~/.bashrc
fi

if ! grep -q $wslInitFname  ~/.bashrc; then
    if yes_no_question "    Do you want to install win10 WSL display settings " ; then
        echo "    Inserting source of $wslInitFname into .bashrc"
        echo "source $installDir/${wslInitFname}" >> ~/.bashrc
    fi
fi

echo "    Done configuring bash"
echo ""
