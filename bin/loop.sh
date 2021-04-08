#!/bin/bash

INTERVAL=$1
shift
while true
do
  $@
  sleep $INTERVAL
done
