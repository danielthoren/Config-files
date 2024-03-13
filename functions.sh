#!/bin/bash

APT_INSTALL="sudo apt -qq install -y"
APT_UPDATE="sudo apt update"
APT_UPGRADE="sudo apt upgrade"

#################################################################################
#  Color functions                                                              #
#################################################################################

bold_yellow_prefix='\033[1;33m'
bold_green_prefix='\033[1;32m'
bold_red_prefix='\033[1;31m'

color_suffix='\033[00m'

print_yellow() {
    printf "${bold_yellow_prefix}$1${color_suffix} \n"
}

print_red() {
    printf "${bold_red_prefix}$1${color_suffix} \n"
}

print_green() {
    printf "${bold_green_prefix}$1${color_suffix} \n"
}

#################################################################################
#  Git utility                                                                  #
#################################################################################

top_level () {
    git rev-parse --show-toplevel
}

#################################################################################
#  Package management                                                           #
#################################################################################

command_exists () {
    type "$1" &> /dev/null ;
}

update() {
    if ! command_exists dpkg-query ; then
        print_red "Command 'dpkg-query' does not exist"
        return 1
    fi

    if ! $updated; then
        $APT_UPDATE 2>&1 >/dev/null
        export updated=true
    fi
}

upgrade() {
    if ! command_exists dpkg-query ; then
        print_red "Command 'dpkg-query' does not exist"
        return 1
    fi

    if ! $upgraded; then
        $APT_UPGRADE  2>&1 >/dev/null
        export upgraded=true
    fi
}

is_package_installed() {
    if ! command_exists dpkg-query ; then
        print_red "Command 'dpkg-query' does not exist"
        return 1
    fi

    return $(dpkg-query -W --showformat='${Status}\n' $1 | grep -q "install ok installed")
}

install() {
    if ! command_exists apt ; then
        print_red "Command 'apt' does not exist"
        return 1
    fi

    update
    $APT_INSTALL "$1" 2>&1 /dev/null

    ## Check exit code of last command to see if install failed
    if [[ $? > 0 ]]; then
        print_red "Failed to install $1"
        return 1
    fi

    print_green "Installed $1"
    return 0
}

install_all() {
    while read line; do
        result=""
        words=( $line )
        if ! is_package_installed $1 ; then
            if [[ $(echo $line | wc -w) = 2 ]]; then
                install ${words[1]}
            else
                install ${words[0]}
            fi
        fi
    done < $1
}

add_source () {
    echo $1

    if ! grep -q "^deb .*$1" /etc/apt/sources.list /etc/apt/sources.list.d/*; then
        sudo add-apt-repository the_ppa 2>&1 >/dev/null

        ## Check exit code of last command to see if install failed
        if [[ $? > 0 ]]; then
            print_red "Failed to add PPA: ${the_ppa}"
            return 1
        fi

        print_green "Added PPA: ${the_ppa}"
        sudo apt update
    fi

    return 0
}

flag_exists () {
    [[ -n ${booleans[$1]} || -z ${booleans[$1]-foo} ]]
}

#Checks if in wsl (windows subsystem linux)
in_wsl () {
    grep -qi microsoft /proc/version
}

init_repo () {
    bash init_repo.sh
}
