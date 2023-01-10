#!/bin/bash

APT_INSTALL="sudo apt -qq install -y"
APT_UPDATE="sudo apt update"
APT_UPGRADE="sudo apt upgrade"

command_exists () {
    type "$1" &> /dev/null ;
}

update() {
    if ! $updated; then
    $APT_UPDATE
    export updated=true
    fi
}

upgrade() {
    if ! $upgraded; then
    $APT_UPGRADE
    export upgraded=true
    fi
}

install() {
    is_pkg_installed=$(dpkg-query -W --showformat='${Status}\n' $1 | grep "install ok installed")

    if ! [[ "${is_pkg_installed}" == "install ok installed" ]]; then
    echo "$1 not installed, installing..."

    update
    $APT_INSTALL "$1"
    fi
    echo "$output"
}

add_source () {
    echo $1

    if ! grep -q "^deb .*$1" /etc/apt/sources.list /etc/apt/sources.list.d/*; then
    echo "Adding repository ${the_ppa} to sources list..."
    sudo add-apt-repository the_ppa
    sudo apt update
    fi
}

flag_exists () {
    [[ -n ${booleans[$1]} || -z ${booleans[$1]-foo} ]]
}

#Checks if in wsl (windows subsystem linux)
in_wsl () {
    grep -qi microsoft /proc/version
}
