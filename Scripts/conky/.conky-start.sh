#!/bin/sh
sleep 5
conky -q -c /home/sync0/Scripts/conky/time &
conky -q -c /home/sync0/Scripts/conky/system &
conky -q -c /home/sync0/Scripts/conky/productivity & exit
