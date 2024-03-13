#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=$HOME
fname='.gdbinit'

source $workingDir/../functions.sh
source $workingDir/../commandParser.sh -scope gdb "$@"

print_green "Configuring gdb in $dir"

install_all $workingDir/deps.txt

if [ -f ${dir}/${fname} ]; then
    echo "  gdbinit link exists, removing..."
    rm "${dir}/${fname}"
fi

ln -s "$workingDir/gdb_files/gdbinit" "${dir}/${fname}"

echo "  Done configuring gdb"
echo ""
