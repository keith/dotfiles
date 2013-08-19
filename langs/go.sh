#!/usr/bin/env bash

if ! which go &> /dev/null; then
    echo "You must install go before install it's modules"
    exit
fi

function install () {
    go get -u github.com/jstemmer/gotags
}

function die () {
    echo "Usage ./$(basename $0) install"
    exit
}

if [[ $# != 1 ]];then
    die
fi

if [[ $1 == "install" ]];then
    install
else
    die
fi

