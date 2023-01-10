#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=$HOME
fname='.gdbinit'

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope gdb "$@"

if ! command_exists gdb ; then
    if flags_exists no-sudo ; then
        echo "gdb not installed, cant install without sudo, exiting..."
        exit no-sudo
    fi

    echo "gdb not installed, installing..."
    update
    install gdb
fi

echo "Configuring gdb in $dir"

if [ -f ${dir}/${fname} ]; then
    echo "gdbinit link exists, removing..."
    rm "${dir}/${fname}"
fi

ln -s "$workingDir/gdb_files/gdbinit" "${dir}/${fname}"

echo "Done configuring gdb"
