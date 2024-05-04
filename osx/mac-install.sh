#!/usr/bin/env bash
# This is a script for bootstrapping macOS setup

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd "$script_dir"/..

if [[ ! -e ./manage.sh ]]; then
  echo "error: script cd broken somehow" >&2
  exit 1
fi

./manage.sh install

if [[ ! -e "$HOME/.bashrc" ]]; then
  echo "error: looks like the manage script failed, try and run it manually" >&2
  exit 1
fi

if ! command -v brew &> /dev/null; then
  echo "error: you need to install homebrew" >&2
  exit 1
fi

# Set correct netrc permissions
touch "$HOME/.netrc"
chmod 0600 "$HOME/.netrc"

# Add Terminal.app theme
open ./osx/parsec.terminal

# Install some default software
brew bundle --file="./osx/Brewfile"
brew bundle --file="./osx/Brewfile.cask"

# Set many default settings
./bin/corners enable
./bin/dock-setup
./bin/gpg-perms
./osx/defaults.sh
