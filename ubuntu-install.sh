#!/usr/bin/env bash

sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo add-apt-repository ppa:gnome-terminator

sudo apt-get update
sudo apt-get -y upgrade
sudo apt-get -y install curl libcurl3 libcurl3-dev php5-curl gconf-editor zlib1g-dev openssl libopenssl-ruby1.9.1 libssl-dev build-essential libruby1.9.1 libreadline-dev git-core git clang gnustep gnustep-devel terminator tmux vim zsh 

chsh -s $(which zsh) $USER
if [[ ! -z "$SUDO_USER" ]];then
    chsh -s $(which zsh) $SUDO_USER
fi


