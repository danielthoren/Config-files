#!/bin/bash

dir=~/.config/regolith

source ../functions.sh

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

echo "Is this a laptop? [y/n]"
read laptop

if [ $laptop == "y" ]; then
    ln -s "${PWD}/regolith_files/laptop/i3xrocks" "${dir}"

    echo "Installing acpi for battery status..."
    sudo apt install acpi
    
else
    ln -s "${PWD}/regolith_files/stationary/i3xrocks" "${dir}"
fi

#Setup i3xblocks scripts
git submodule update --init --recursive

ln -s "${PWD}/regolith_files/scripts" "${dir}"

cd "${dir}/scripts/i3blocks-contrib"
make

echo -e "Done configuring regolith"
