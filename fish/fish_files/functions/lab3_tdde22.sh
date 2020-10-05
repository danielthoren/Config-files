#!/bin/bash

~/.config/fish/functions/fix_encoding.sh

cp /home/daniel5908/git/TDDE22_labs/2020_ht/lab3/Lab3/src/Point.java ./
cp -r /home/daniel5908/git/TDDE22_labs/2020_ht/lab3/Lab3/src/data ./

sed 's/.*package.*;/ /' -i *.java

javac Fast.java
java Fast data/mystery10089.txt
java Fast data/grid6x6.txt

rm Point.java
rm -r data
rm *.class
rm *~

