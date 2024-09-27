#!/bin/bash

set -euo pipefail

sudo apt-add-repository -y ppa:git-core/ppa

sudo apt install -y \
  ansifilter \
  bash \
  build-essential \
  cmake \
  colordiff \
  curl \
  fd-find \
  forkstat \
  fzf \
  fzy \
  gdb \
  gettext \
  gh \
  git \
  golang \
  htop \
  kitware-archive-keyring \
  libedit-dev \
  ninja-build \
  parallel \
  patchelf \
  pax-utils \
  python3 \
  python3-pip \
  python3-venv \
  ripgrep \
  speedtest-cli \
  sudo \
  systemd-coredump \
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
