#!/usr/bin/env bash

sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo add-apt-repository ppa:gnome-terminator
sudo apt-add-repository ppa:mizuno-as/silversearcher-ag

sudo apt-get update
sudo apt-get -y upgrade
sudo apt-get -y install curl subversion libcurl3 libcurl3-dev php5-curl gconf-editor zlib1g-dev openssl libopenssl-ruby1.9.1 libssl-dev build-essential libruby1.9.1 libreadline-dev git-core git clang silversearcher-ag terminator tmux vim vim-gnome zsh xclip

chsh -s $(which zsh) $USER
if [[ ! -z "$SUDO_USER" ]];then
    chsh -s $(which zsh) $SUDO_USER
fi


