#!/bin/bash

bash_configure ()
{
    local workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
    local dir=$HOME
    local fname='.bash_init.sh'

    # Make sure common bash is installed
    source "$workingDir/../common_bash/conf.sh"

    echo "Configuring bash in $dir"

    if [ -f "${dir}/${fname}" ]; then
        echo "Bash config link exists, removing..."
        rm "${dir}/${fname}"
    fi

    ln -s "$workingDir/bash_files/bash_init.sh" "${dir}/${fname}"

    if ! grep $fname  ~/.bashrc; then
        echo "Inserting source of .bash_init.sh into .bashrc"
        echo "source $dir/${fname}" >> ~/.bashrc
    fi

    echo "Done configuring bash"
}
bash_configure
