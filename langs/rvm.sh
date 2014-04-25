#!/usr/bin/env bash

rvmpath="$HOME/.rvm"

function install () {
    if [[ -d $rvmpath ]];then
        echo "$HOME/.rvm already exists"
        exit
    fi

    curl -sSL https://get.rvm.io | bash

    exec $SHELL -l
}

function link () {
    local path=$rvmpath/gemsets/default.gems
    rm $path
    ln -s $DOTFILES/default-gems $path
}

function die () {
    echo "Usage ./$(basename $0) {install|remove|link}"
    exit
}

if [[ $# != 1 ]]; then
    die
fi

if [[ $1 == "install" ]]; then
    install
    link
elif [[ $1 == "remove" ]]; then
    rm -rf $rvmpath
elif [[ $1 == "link" ]]; then
    link
else
    die
fi
