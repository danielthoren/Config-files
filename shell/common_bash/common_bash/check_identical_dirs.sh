#!/bin/bash

workingDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
dir="$HOME/.config/common_bash"

source $workingDir/../../../functions.sh

srcDir=$1
destDir=$2

srcMd5File="srcDir.md5Sum"
destMd5File="destDir.md5Sum"

find "$srcDir"  -type f | parallel -j 16 --progress md5sum > $srcMd5File &
P1=$!
find "$destDir" -type f | parallel -j 16 --progress md5sum > $destMd5File &
P2=$!

wait $P1 $P2

sort -k 2 $srcMd5File > $srcMd5File
sort -k 2 $destMd5File > $destMd5File

sed -i "s:'$srcDir':\/:g" $srcMd5File
sed -i "s:'$destDir':\/:g" $destMd5File

dirDiff=$(diff $srcMd5File $destMd5File)

echo ""
if $dirDiff ; then
    print_green "diff: Directories are identical"
else
    print_red "diff: Directories differ: "
    echo ""
    echo $dirDiff
    echo ""
fi

dirCmp=$(cmp $srcMd5File $destMd5File)
if $dirCmp ; then
    print_green "cmp:  Directories are identical"
else
    print_red "cmp:  Directories differ: "
    echo ""
    echo $dirDiff
    echo ""
fi
