#!/bin/bash

path=/home/daniel5908/git/TDDE22_labs/lab4/

for i in $(find $path -name "*.java")
do
    name=$(basename "$i")

    if [[ ! -f $name ]]
    then
	echo "Copying $i..."
	cp $i $name
    fi
done

cp -r "${path}tdde22_wordchain_test" ./

for i in $(find ./ -name "*.java")
do
    name=$(basename "$i")

    sed -i 's/package/\/\/package/g' $name
done

javac *.java
cat tdde22_wordchain_test | java Main

for i in $(find ./ -name "*.java")
do
    name=$(basename "$i")

    sed -i 's/\/\/package/package/g' $name
done
