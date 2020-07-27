#!/bin/bash

APT_INSTALL="sudo apt -qq install -y"
APT_UPDATE="sudo apt update"

command_exists () {
    type "$1" &> /dev/null ;
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
