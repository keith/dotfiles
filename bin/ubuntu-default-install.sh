#!/bin/bash

set -euo pipefail

sudo apt-add-repository -y ppa:git-core/ppa

sudo apt purge --auto-remove cmake lib-nodedev || true
wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | sudo tee /etc/apt/trusted.gpg.d/kitware.gpg >/dev/null
sudo apt-add-repository 'deb https://apt.kitware.com/ubuntu/ jammy main'

curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -

sudo apt install -y \
  ansifilter \
  bash \
  build-essential \
  cmake \
  colordiff \
  curl \
  fd-find \
  forkstat \
  fzy \
  gdb \
  gettext \
  git \
  golang \
  htop \
  kitware-archive-keyring \
  libedit-dev \
  ninja-build \
  nodejs \
  patchelf \
  ripgrep \
  speedtest-cli \
  sudo \
  tmux \
  tree \
  universal-ctags \
  unzip \
  urlview \
  vim \
  zsh

sudo rm -f /usr/share/keyrings/kitware-archive-keyring.gpg
sudo apt autoremove

go install github.com/bazelbuild/bazelisk@latest
go install github.com/bazelbuild/buildtools/buildifier@latest
go install github.com/bazelbuild/buildtools/buildozer@latest

sudo npm install -g bash-language-server

pip3 install cmake-language-server
pip3 install neovim
pip3 install pyright

cd /tmp
git clone https://github.com/neovim/neovim
cd neovim
make CMAKE_BUILD_TYPE=Release
sudo make install

cd /tmp
git clone https://github.com/keith/tag
cd tag
cmake -B build
cmake --build build
sudo cmake --install build
