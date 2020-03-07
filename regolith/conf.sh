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
fi

ln -s "${PWD}/regolith_files/i3" "${dir}"
ln -s "${PWD}/regolith_files/i3xrocks" "${dir}"

echo -e "Done configuring regolith"
