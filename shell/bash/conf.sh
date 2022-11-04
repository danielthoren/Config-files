#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir=$HOME
fname='.bash_init.sh'

# Make sure common bash is installed
bash "$workingDir/../common_bash/conf.sh"

echo "Configuring bash in $dir"

if [ -f ${dir}/${fname} ]; then
    echo "Bash config link exists, removing..."
    rm "${dir}/${fname}"
fi

ln -s "$workingDir/bash_files/bash_init.sh" "${dir}/${fname}"

if ! grep $fname  ~/.bashrc; then
    echo "source $dir/${fname}" >> ~/.bashrc
fi

echo "Done configuring bash"
