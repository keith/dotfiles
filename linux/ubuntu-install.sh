#!/usr/bin/env bash

sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo add-apt-repository ppa:gnome-terminator
sudo apt-add-repository ppa:mizuno-as/silversearcher-ag
sudo add-apt-repository ppa:chris-lea/node.js

sudo apt-get update
sudo apt-get -y upgrade
sudo apt-get -y install autoconf automake bison build-essential clang curl exuberant-ctags g++ gconf-editor git git-core libc6-dev libcurl3 libcurl3-dev libopenssl-ruby1.9.1 libreadline-dev libreadline6 libreadline6-dev libruby1.9.1 libsqlite3-dev libssl-dev libtool libxml2-dev libxslt-dev libyaml-dev make mutt-patched ncurses-dev nodejs openssl php5-curl python python-software-properties secure-delete silversearcher-ag sqlite3 subversion terminator tmux vim vim-gnome xclip zlib1g zlib1g-dev zsh

chsh -s $(which zsh) $USER
if [[ ! -z "$SUDO_USER" ]];then
    chsh -s $(which zsh) $SUDO_USER
fi

