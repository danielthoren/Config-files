#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo "Installing fish and bash files"

source "$workingDir/common_bash/conf.sh" -f
source "$workingDir/fish/conf.sh"
source "$workingDir/bash/conf.sh"
