#!/bin/bash

SERVER_CONF_FILE=~/.config/fish/server_conf

declare -a array=()
i=0

while read -r line; do
    array[i]=$line
    let "i++"
done < $SERVER_CONF_FILE

if ! [ "$i" == 5 ]; then

    printf "There must be three lines in the server_conf file
    	   0: user on server
    	   1: server password
    	   2: local server ip
	   3: global server ip
	   4: port
	   
	   aborting..."

    exit -1
fi

user="${array[0]}"
passw="${array[1]}"
local_ip="${array[2]}"
global_ip="${array[3]}"
ssh_port="${array[4]}"
