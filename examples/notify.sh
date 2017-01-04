#!/bin/bash

if [ -z "$1" ]; then 
    TIME=600;  
else 
    TIME=$1;
fi

while true; do
    notify-send "$(hapidry notify -Aq)"
    sleep $TIME
done
