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
rm -rf $HOME/.rbenv
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
brew update

brew install bash cloc git grep heroku-toolbelt hub imagemagick llvm lynx macvim markdown mercurial mogenerator node openssh openssl python python3 readline reattach-to-user-namespace rsync ruby-build sqlite the_silver_searcher tmux tree valgrind vim wget zsh zsh-completions  
brew install --HEAD brew-cask

./manage.sh install &
wait

git clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
git clone https://github.com/sstephenson/rbenv-default-gems.git $HOME/.rbenv/plugins/rbenv-default-gems
git clone git://github.com/sstephenson/ruby-build.git $HOME/.rbenv/plugins/ruby-build
git clone https://github.com/sstephenson/rbenv-gem-rehash.git $HOME/.rbenv/plugins/rbenv-gem-rehash
git clone git://github.com/tpope/rbenv-readline.git $HOME/.rbenv/plugins/rbenv-readline

