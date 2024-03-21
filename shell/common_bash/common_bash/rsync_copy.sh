#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/common_bash"

source $workingDir/../../../functions.sh

srcDir=$1
destDir=$2

rsync -axHAXh --info=progress2 $srcDir $destDir

echo ""
print_green "Rsync complete, checking diff..."

bash $workingDir/check_identical_dirs.sh $1 $2
