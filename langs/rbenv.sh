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
  git clone https://github.com/tpope/rbenv-ctags.git $rbenvpath/plugins/rbenv-ctags
  git clone https://github.com/rkh/rbenv-update.git $rbenvpath/plugins/rbenv-update

  ln -s $DOTFILES/default-gems $rbenvpath

  source $DOTFILES/langs/ruby.bash
  bundler-cores.sh
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
