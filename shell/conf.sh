#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

source $workingDir/../functions.sh

print_green "Configuring shell"

bash "$workingDir/common_bash/conf.sh" "$@"
bash "$workingDir/fish/conf.sh" "$@"
bash "$workingDir/bash/conf.sh" "$@"

echo "Done configuring shell"
