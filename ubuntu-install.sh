#!/usr/bin/env bash

sudo -v
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo apt-get update
sudo -y apt-get upgrade
sudo -y apt-get install tmux vim zsh 

chsh -s $(which zsh) $USER
if [[ ! -z "$SUDO_USER" ]];then
    chsh -s $(which zsh) $SUDO_USER
fi


