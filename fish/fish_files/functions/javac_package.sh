#!/bin/bash

path=./

for i in $(find $path -name "*.java")
do
    name=$(basename "$i")

    if [[ ! -f $name ]]
    then
	echo "Copying $i..."
	cp $i $name
    fi
done

for i in $(find ./ -name "*.java")
do
    name=$(basename "$i")
    
    sed -i 's/package/\/\/package/g' $name
done

javac *.java

for i in $(find ./ -name "*.java")
do
    name=$(basename "$i")

    sed -i 's/\/\/package/package/g' $name
done
