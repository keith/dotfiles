#!/usr/bin/env bash
# This is a script for bootstrapping OS X setup

set -e

if [[ ! -e "../manage.sh" ]]; then
  echo "Make sure you have the manage script nearby"
  exit 1
fi

cd "$(dirname "$0")"
cd ..
./manage.sh install

if [[ ! -e "$HOME/.bashrc" ]]; then
  echo "Looks like the manage script failed, try and run it manually"
  exit 1
fi

if ! which brew &> /dev/null; then
  echo "You need to install homebrew"
  exit 1
fi

open "$DOTFILES/osx/parsec.terminal"
"$DOTFILES/osx/defaults.sh"

brew tap Homebrew/bundle
brew bundle --file="$DOTFILES/osx/Brewfile"
brew bundle --file="$DOTFILES/osx/Brewfile.cask"
