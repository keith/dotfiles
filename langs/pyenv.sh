#!/usr/bin/env bash

pyenvpath="$HOME/.pyenv"

function install () {
    if [[ -d $pyenvpath ]];then
        echo "$pyenvpath already exists"
        exit
    fi

    git clone git://github.com/yyuu/pyenv.git $pyenvpath
    git clone https://github.com/yyuu/pyenv-pip-rehash.git $pyenvpath/plugins/pyenv-pip-rehash
    git clone git://github.com/yyuu/pyenv-update.git $pyenvpath/plugins/pyenv-update

    exec $SHELL -l
}

function die () {
    echo "Usage ./$(basename $0) {install|remove}"
    exit
}

if [[ $# != 1 ]]; then
    die
fi

if [[ $1 == "install" ]]; then
    install
elif [[ $1 == "remove" ]]; then
    rm -rf $pyenvpath
else
    die
fi
