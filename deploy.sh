#!/bin/sh

CURRENT=$(pwd)

mkdir -p ~/backup/emacs
mkdir -p ~/local/bin
mkdir -p ~/local/config
mkdir -p ~/close/bin
mkdir -p ~/tmp/emacs
touch ~/local/.abbrev_defs

echo $SHELL | grep -q zsh
if [ $? -ne 0 ]; then
    chsh -s $(grep zsh /etc/shells | head -n 1)
fi

find . -maxdepth 1 -name '.*' | \
    sed 's!^./!!' | \
    grep -v '^\.$' | \
    grep -v '^\.git$' | \
    grep -v '^\.gitignore$' | \
    grep -v '~$' | \
    xargs -IXXXX ln -s XXXX ~/

if [ ! -f ~/local/local.el ]; then
    touch ~/local/local.el
fi

if [ ! -f ~/local/.zsh_local ]; then
    touch ~/local/.zsh_local
fi

if [ ! -d ~/.cask ]; then
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

if [ ! -f ~/local/bin/peco ]; then
    cd ~/tmp
    wget https://github.com/peco/peco/releases/download/v0.5.0/peco_linux_amd64.tar.gz
    tar xvzf peco_linux_amd64.tar.gz
    cp peco_linux_amd64/peco ~/local/bin/
    chmod +x ~/local/bin/peco
    cd $CURRENT
fi
