#!/bin/bash

#If in WSL, change display settings to use windows x-server

if ! grep -q WSL2 /proc/version; then
    exit 0
fi

export DISPLAY=$(route.exe print | grep 0.0.0.0 | head -1 | awk '{print $4}'):0.0
export LIBGL_ALWAYS_INDIRECT=1
