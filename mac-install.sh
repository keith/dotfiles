#!/usr/bin/env bash
# This is an ugly script for bootstrapping OS X setup

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

if [[ ! -e "$HOME/.bashrc" ]]; then
    echo "Looks like the manage script failed, try and run it manually"
    exit
fi

source "$HOME/.bashrc"

if ! which brew &> /dev/null; then
    echo "Homebrew is not installed in your \$PATH"
    exit
fi

brew tap homebrew/dupes
brew tap phinze/homebrew-cask
brew update &
wait

formulas=(bash cloc git grep heroku-toolbelt hub imagemagick llvm macvim mercurial mogenerator node openssh openssl python python3 readline reattach-to-user-namespace sqlite the_silver_searcher tmux tree valgrind vim wget zsh zsh-completions)

for f in ${formulas[@]}
do
  brew install $f
done

brew install --HEAD brew-cask

./rbenv.sh install

