#!/bin/bash

common_bash_dir ()
{
    echo "$HOME/.config/common_bash"
}

common_bash_working_dir ()
{
    echo $( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
}

common_bash_configure ()
{
    local workingDir=$(common_bash_working_dir)
    local dir=$(common_bash_dir)

    echo "Configuring common bash in $dir"

    if [ -d $dir ]; then
        echo "Folder exists, deleting..."
        rm -r "${dir}"
    fi

    ln -s "$workingDir/common_bash" $dir

    echo "Done configuring common bash files in $dir"
}

common_bash_is_configured ()
{
    local dir=$(common_bash_dir)
    if [ ! -d $dir ]; then
        # 0 = true
        return 0
    else
        # 1 = false
        return 1
    fi
}

common_bash_conf ()
{
    local workingDir=$(common_bash_working_dir)

    source "$workingDir/../../functions.sh"
    source "$workingDir/../../commandParser.sh" "$@"

    if flag_exists f ; then
        common_bash_configure
    elif common_bash_is_configured; then
        common_bash_configure
    fi
}

common_bash_conf "$@"
