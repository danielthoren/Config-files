#!/bin/bash

global_ip="155.4.147.233"
sshPort=2022
user=daniel5908

echo "Enter password:"
read -s password

konsole -e sshpass -p $password ssh -p $sshPort -L 8081:192.168.1.100:8081 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 80:192.168.1.100:80 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 8112:192.168.1.100:8112 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 5050:192.168.1.100:5050 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 5000:192.168.1.214:5000 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 8006:192.168.1.7:8006 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 9117:192.168.1.100:9117 $user@$global_ip &
konsole -e sshpass -p $password ssh -p $sshPort -L 8080:192.168.1.100:8080 $user@$global_ip &

