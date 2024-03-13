#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=~/.emacs.d

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh "$@"

print_green "Configuring emacs in folder $dir"

add_source ppa:kelleyk/emacs

install_all $workingDir/deps.txt

if [ ! -d $dir ]; then
    echo "  Folder does not exist, creating folder"
    mkdir -p $dir
else
    echo "  Folder exists, purging data"
    rm "${dir}/init.el"
    rm -r "${dir}/functions"
    rm -r "${dir}/settings"
    rm -r "${dir}/gendoxy"
    rm -r "${dir}/Block-Comment-Mode"
fi

echo "  Initalizing submodules"
git submodule init 2>&1 >/dev/null
git submodule update 2>&1 >/dev/null

if [[ -d "$workingDir/emacs_files/gendoxy" ]]; then
    cd ./emacs_files/gendoxy
    if ! [[ $(git apply --check ../gendoxy-change-template.patch 2>&1 | grep "error") ]]; then
        echo "  Applying gendoxy patch"
        git apply ../gendoxy-change-template.patch
    else
        echo "  gendoxy patch already applied "
    fi
    cd ..
else
    print_red "  gendoxy dir not found"
fi

ln -s "$workingDir/emacs_files/settings" "${dir}"
ln -s "$workingDir/emacs_files/functions" "${dir}"
ln -s "$workingDir/emacs_files/gendoxy" "${dir}"
ln -s "$workingDir/emacs_files/Block-Comment-Mode" "${dir}"

ln -s "$workingDir/emacs_files/init.el" "${dir}"

echo "  Done configuring emacs"
echo ""
