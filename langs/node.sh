#!/usr/bin/env bash

function install () {
    nodes=(bower coffee-script grunt-cli)

    for n in ${nodes[@]}
    do
        npm install -g $n
    done
}

function die () {
    echo "Usage ./$(basename $0) install"
    exit
}

if ! which npm &> /dev/null;then
    echo "You must install node before installing its packages"
    exit
fi

if [[ $# != 1 ]]; then
    die
fi

if [[ $1 == "install" ]]; then
    install
else
    die
fi

