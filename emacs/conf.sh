#!/bin/bash


dir=~/.emacs.d/

if [! -d $dir ]; then
   mkdir -p $dir
else
    rm "${dir}/init.el"
fi

ln -s "${PWD}/emacs_files/settings" "${dir}"
ln -s "${PWD}/emacs_files/funs" "${dir}"
ln -s "${PWD}/emacs_files/init.el" "${dir}"
