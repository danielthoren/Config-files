#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope regolith "$@"

dir=~/.config/regolith

echo "Configuring regolith in folder ${dir}"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging data"
    rm -r "${dir}/i3"
    rm -r "${dir}/i3xrocks"
    rm -r "${dir}/scripts"
fi

ln -s "$workingDir/regolith_files/i3" "${dir}"

if flag_exists laptop ; then
    ln -s "$workingDir/regolith_files/laptop/i3xrocks" "${dir}"

    if flag_exists no-sudo ; then
        echo "acpi for battery status not installed, cant install without sudo..."
    else
        echo "Installing acpi for battery status..."
        update
        install acpi
    fi

else
    ln -s "$workingDir/regolith_files/stationary/i3xrocks" "${dir}"
fi

#Setup i3xblocks scripts
git submodule update --init --recursive

ln -s "$workingDir/regolith_files/scripts" "${dir}"

cd "${dir}/scripts/i3blocks-contrib"
make

echo -e "Done configuring regolith"
