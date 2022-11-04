#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

echo "Installing fish and bash files"

bash "$workingDir/common_bash/conf.sh" "$@"
bash "$workingDir/fish/conf.sh" "$@"
bash "$workingDir/bash/conf.sh" "$@"
