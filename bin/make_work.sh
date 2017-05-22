if [ $# -eq 0 ]; then
    dir=~/tmp/$(date '+%Y%m%d_%H%M')
else
    dir=~/tmp/$(date '+%Y%m%d_%H%M')_"$1"
fi

dir=~/tmp/$(date '+%Y%m%d_%H%M%S')
mkdir -p $dir
cd $dir

# change dir of terminal
exec $(SHELL)
