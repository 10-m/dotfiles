#!/bin/bash
#http://qiita.com/yushimizu/items/5cbc1b9c940cc4a9f054

i=0
while :
do
    i=`expr $i + 1`
    echo "Now Processing... line :${i}"
    for j in `seq 1 1 20`
    do
        echo -en '|\r' ; sleep 0.05;
        echo -en '/\r' ; sleep 0.05;
        echo -en '-\r' ; sleep 0.05;
    done
    sleepTime=`expr $RANDOM % 3`
    sleep "${sleepTime}s"
done
