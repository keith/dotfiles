#!/bin/bash

set -euo pipefail

# TODO: Neovim

curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -

sudo apt install \
  ansifilter \
  bash \
  build-essential \
  cmake \
  colordiff \
  curl \
  fd-find \
  forkstat \
  fzy \
  gettext \
  git \
  golang \
  htop \
  ninja-build \
  nodejs \
  ripgrep \
  sudo \
  tmux \
  universal-ctags \
  unzip \
  vim \
  zsh

go install github.com/bazelbuild/bazelisk@latest
go install github.com/bazelbuild/buildtools/buildifier@latest
go install github.com/bazelbuild/buildtools/buildozer@latest

sudo npm install -g bash-language-server
sudo npm install -g pyright
