#!/bin/bash

src=ddtest_1.txt
dest=ddtest_2.txt

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

[ "$srcSum" == "$destSum" ] && echo "Success" || echo "Fail"
