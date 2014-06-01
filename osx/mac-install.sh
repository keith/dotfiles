#!/usr/bin/env bash
# This is a script for bootstrapping OS X setup

if ! which xcodebuild &> /dev/null; then
    echo "Attempting to install the Xcode developer tools"
    $(xcode-select --install)
fi

if [[ ! -e "../manage.sh" ]]; then
    echo "Make sure you have the manage script nearby"
    exit
fi

cd $(dirname $0)
cd ..
./manage.sh install

if [[ ! -e "$HOME/.bashrc" ]]; then
    echo "Looks like the manage script failed, try and run it manually"
    exit
fi

source "$HOME/.bashrc"

if ! which brew &> /dev/null; then
    echo "Installing homebrew"
    mkdir -p /usr/local
    sudo chown -R $(whoami) /usr/local
    ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
fi

brew bundle $DOTFILES/osx/Brewfile

./install-langs.sh install
