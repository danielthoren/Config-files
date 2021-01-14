#!/bin/bash

source $FIFUNC/fish/fish_files/functions/read_server_config.sh

konsole -e sshpass -p $passw ssh -p $ssh_port -L 8081:192.168.1.100:8081 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 80:192.168.1.100:80 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 8112:192.168.1.100:8112 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 5050:192.168.1.100:5050 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 5000:192.168.1.214:5000 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 8006:192.168.1.7:8006 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 9117:192.168.1.100:9117 $user@$global_ip &
konsole -e sshpass -p $passw ssh -p $ssh_port -L 8080:192.168.1.100:8080 $user@$global_ip &

