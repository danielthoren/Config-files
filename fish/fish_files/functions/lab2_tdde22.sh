#!/bin/bash

source $FIFUNC/functions.sh
source $FIFUNC/commandParser.sh "$@"

~/.config/fish/functions/fix_encoding.sh

cp /home/daniel5908/labbass/TDDE22/tdde22-staff/AVL/src/* .
sed -i "s/.*package.*;/ /g" AVLTreeNode.java
javac Lab2Grading.java
rm -f *.java

if flag_exists v ; then
    java Lab2Grading -v
else
    java Lab2Grading
fi

rm -f *.class monsterdata.txt
cp .originalsubmission/* .
