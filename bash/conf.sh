#!/bin/bash

dir=~

echo "Configuring bash in $dir"

ln -s "${PWD}/bash_files/bash_init.sh" "${dir}/.bash_init.sh"

echo "source $dir/.bash_init.sh" >> ~/.bashrc
