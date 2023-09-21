#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
source $workingDir/../../functions.sh

warningWords="FIXME|TODO|NOTE"

noteSedRule=''/NOTE/s//$(printf "${bold_green_prefix}NOTE${color_suffix}")/''
todoSedRule=''/TODO/s//$(printf "${bold_yellow_prefix}TODO${color_suffix}")/''
fixmeSedRule=''/FIXME/s//$(printf "${bold_red_prefix}FIXME${color_suffix}")/''

additions=$(git diff --cached | \
                grep -E "^\+" | \
                sed $noteSedRule | \
                sed $todoSedRule | \
                sed $fixmeSedRule)
currFile=""
hasKeyWords=false

while read -r line; do
    if [[ $(echo $line | grep '+++ b/') ]]
    then
        currFile=$line
    elif [[ $(echo $line | egrep -l $warningWords) ]]
    then
        echo "${currFile:6} : ${line:1}"
        hasKeyWords=true
    fi
done <<< $additions

if [ "$hasKeyWords" = true ]
then
    printf "\n"
    exec < /dev/tty #"Redirect" user input to this current terminal
    yn=""
    read -p "Would you like to proceed with your commit anyway? [y/n]:" yn

    case $yn in
        [Yy]* )
            print_green "Continuing with commit"
            ;;
        [Nn]* )
            print_yellow "Aborting commit"
            exit 1
            ;;
        *)
            print_red "Invalid input, please awnser with y or n"
            exit 1
            ;;
    esac
fi
