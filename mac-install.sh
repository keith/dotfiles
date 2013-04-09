#!/usr/bin/env bash

if ! which xcodebuild &> /dev/null; then
    echo "You need to install the Xcode Command Line Tools before running this script"
    exit
fi

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)" &
wait

if [[ ! -e "manage.sh" ]]; then
    echo "Make sure you have the manage script in the same directory!"
    exit
fi

./manage.sh install &
wait
./osx.txt &
wait

if [[ ! -e "$HOME/.bashrc" ]]; then
    echo "Looks like the manage script failed, try and run it manually"
    exit
fi

source "$SHELL/.bashrc"

if ! which brew &> /dev/null; then
    echo "Homebrew is not installed in your \$PATH"
    exit
fi

brew install appledoc automake bash bash-completion curl git heroku-toolbelt hub imagemagick llvm lynx macvim make markdown mercurial mogenerator mysql node openssh openssl postgresql rbenv rbenv-default-gems rbenv-gem-rehash rsync ruby-build tree vim wget zsh zsh-completions

rbenv install 1.9.3-p385 2.0.0-p0

