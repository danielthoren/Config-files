#!/bin/bash

~/.config/fish/functions/fix_encoding.sh

cp /home/daniel5908/labbass/TDDE22/tdde22-staff/Hashtable/src/*.java .
sed 's/.*package.*;/ /' -i SymbolTable.java
javac Lab1Grading.java
rm -f Lab1Grading.java TableTest.java CorrectTable.java
java Lab1Grading
rm -f *.class

