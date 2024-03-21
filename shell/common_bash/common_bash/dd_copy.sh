#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/common_bash"

source $workingDir/../../functions.sh

src=$1
dest=$2

dd if=$src of=$dest

srcSumRaw=$(sha256sum $src)
destSumRaw=$(sha256sum $dest)

echo ""
echo "sha256sum output:"
echo ""
echo $srcSumRaw
echo $destSumRaw
echo ""

srcSum=$(echo $srcSumRaw | cut -d' ' -f1)
destSum=$(echo $destSumRaw | cut -d' ' -f1)

[ "$srcSum" == "$destSum" ] && print_green "Success" || print_red "Fail"
