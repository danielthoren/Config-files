#!/bin/bash

cp /home/daniel5908/git/TDDE22_labs/2020_ht/lab4/Lab4/tdde22_wordchain_test ./
cp /home/daniel5908/git/TDDE22_labs/2020_ht/lab4/Lab4/Main.java ./
cp /home/daniel5908/git/TDDE22_labs/2020_ht/lab4/Lab4/Queue.java ./

sed 's/.*package.*;/ /' -i *.java

javac Main.java
cat tdde22_wordchain_test | java Main

rm tdde22_wordchain_test
rm Main.java
rm Queue.java
rm *.class
rm *~

