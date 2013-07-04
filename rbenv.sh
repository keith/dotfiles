#!/usr/bin/env bash

function install () {
    if [[ -d $HOME/.rbenv ]];then
        echo "$HOME/.rbenv already exists"
        exit
    fi

    git clone https://github.com/sstephenson/rbenv.git $HOME/.rbenv
    git clone https://github.com/sstephenson/rbenv-default-gems.git $HOME/.rbenv/plugins/rbenv-default-gems
    git clone https://github.com/sstephenson/ruby-build.git $HOME/.rbenv/plugins/ruby-build
    git clone https://github.com/sstephenson/rbenv-gem-rehash.git $HOME/.rbenv/plugins/rbenv-gem-rehash
    git clone https://github.com/tpope/rbenv-readline.git $HOME/.rbenv/plugins/rbenv-readline

    ln -s $(pwd)/default-gems $HOME/.rbenv
}

function die () {
    echo "Usage ./rbenv.sh {install|remove}"
    exit
}

if [[ $# != 1 ]]; then
    die
fi

if [[ $1 == "install" ]]; then
    install
elif [[ $1 == "remove" ]]; then
    rm -rf $HOME/.rbenv
else
    die
fi

