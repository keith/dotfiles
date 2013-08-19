#!/usr/bin/env bash

rbenvpath="$HOME/.rbenv"

function install () {
    if [[ -d $rbenvpath ]];then
        echo "$HOME/.rbenv already exists"
        exit
    fi

    git clone https://github.com/sstephenson/rbenv.git $rbenvpath
    git clone https://github.com/sstephenson/rbenv-default-gems.git $rbenvpath/plugins/rbenv-default-gems
    git clone https://github.com/sstephenson/ruby-build.git $rbenvpath/plugins/ruby-build
    git clone https://github.com/sstephenson/rbenv-gem-rehash.git $rbenvpath/plugins/rbenv-gem-rehash
    git clone https://github.com/tpope/rbenv-readline.git $rbenvpath/plugins/rbenv-readline

    ln -s $DOTFILES/default-gems $rbenvpath
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
    rm -rf $rbenvpath
else
    die
fi

