#!/bin/bash

dir=~/.config/regolith

source ../functions.sh

source ../commandParser.sh -scope regolith "$@"

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

ln -s "${PWD}/regolith_files/i3" "${dir}"

if [ ${array[laptop]+abc} ] && ${booleanArrayName[laptop]} ; then
    ln -s "${PWD}/regolith_files/laptop/i3xrocks" "${dir}"

    if [ ${array[no-sudo]+abc} ] && ${booleanArrayName[no-sudo]} ; then
	echo "acpi for battery status not installed, cant install without sudo..."
    else
	echo "Installing acpi for battery status..."	
	sudo apt install acpi
    fi
    
else
    ln -s "${PWD}/regolith_files/stationary/i3xrocks" "${dir}"
fi

#Setup i3xblocks scripts
git submodule update --init --recursive

ln -s "${PWD}/regolith_files/scripts" "${dir}"

cd "${dir}/scripts/i3blocks-contrib"
make

echo -e "Done configuring regolith"
