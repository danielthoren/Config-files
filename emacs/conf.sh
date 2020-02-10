#!/bin/bash


dir=~/.emacs.d/

echo "Configuring emacs in folder $dir"

if [ ! -d $dir ]; then
    echo "Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "Folder exists, purging data"
    rm "${dir}/init.el"
fi

ln -s "${PWD}/emacs_files/settings" "${dir}"
ln -s "${PWD}/emacs_files/funs" "${dir}"
ln -s "${PWD}/emacs_files/init.el" "${dir}"

echo "Done configuring emacs"
