#!/bin/bash

# http://qiita.com/yushimizu/items/5cbc1b9c940cc4a9f054

#define library name
libraries=("ujthsk" "wtpkty" "dancmu" "ckbbbw" "dgkujy" "rahtzz")
# 1 - 100
progressBarSize=20

while :
do
    library=${libraries[$(($RANDOM % ${#libraries[*]}))]}
    echo "Collecting ${library}"

    fileSize=$((($RANDOM % 1000) + $progressBarSize))
    percentSize=$(($fileSize / $progressBarSize))

    fileName="${library}-"$(($RANDOM % 10))"."$(($RANDOM % 10))"."$(($RANDOM % 10))".tar.gz"
    echo "    Downloading ${fileName} (${fileSize}kB)"

    nowSize=0
    # loop downloading
    while [ $nowSize -lt $fileSize ]
    do
        progressSize=$(($RANDOM % 100))
        nowSize=$(($nowSize + $progressSize))
        sizeQuotient=$(($nowSize / $percentSize))

        # protect over 100 percent
        if [ $nowSize -gt $fileSize ] ; then
            nowSize=$fileSize
            sizeQuotient=$progressBarSize
        fi

        # make progress bar
        progressBar=""
        for i in `seq 1 $sizeQuotient`
        do
            progressBar=$progressBar"#"
        done
        for i in `seq 1 $(($progressBarSize - $sizeQuotient))`
        do
            progressBar=$progressBar" "
        done

        #progress bar
        echo -en $(($sizeQuotient * (100 / $progressBarSize)))"%|${progressBar}| ${nowSize}kB ${progressSize}kB/s \r"

        sleep 1s
    done

    echo
    echo "Installing..."
    for i in `seq 1 1 20`
    do
        echo -en '|\r' ; sleep 0.05;
        echo -en '/\r' ; sleep 0.05;
        echo -en '-\r' ; sleep 0.05;
    done
    echo "Successfully installed: ${fileName}"

    if [ $(($RANDOM % 10)) -lt 1 ] ; then
        echo "Building wheels for collected packages..."
        sleep 3s
    fi
    echo
    sleep 1s
done
