#!/bin/zsh

dir=~/tmp/$(date '+%Y%m%d_%H%M')
mkdir -p $dir
cd $dir
exec zsh
