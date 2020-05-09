#!/bin/bash

cd emacs
./conf.sh -all-conf "$@"
cd ../fish
./conf.sh -all-conf "$@"
cd ../kitty
.conf.sh -all-conf "$@"
cd ../ranger
./conf.sh -all-conf "$@"
cd ../regolith
./conf.sh -all-conf "$@"
