#!/bin/bash -f

find . -name "*.java" | while read FFF
do
    iconv -f ISO-8859-1 -t UTF-8 $FFF > $FFF.NEW
    mv "${FFF}.NEW" "${FFF}"
done
