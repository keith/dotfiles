#!/bin/bash

set -euo pipefail

sudo apt-get update
sudo apt-get install -y ca-certificates gpg wget

wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null \
  | gpg --dearmor - | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null

echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main" \
  | sudo tee /etc/apt/sources.list.d/kitware.list >/dev/null

sudo apt-add-repository -y ppa:git-core/ppa
sudo apt-get update

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
  jq \
  kitware-archive-keyring \
  libedit-dev \
  libssl-dev \
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
  zip \
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
readonly nvim_version=0.10.2
curl -L https://github.com/neovim/neovim/archive/refs/tags/v$nvim_version.tar.gz | tar zxf -
cd neovim-$nvim_version
make CMAKE_BUILD_TYPE=Release
sudo make install

cd /tmp
git clone https://github.com/keith/tag
cd tag
cmake -B build
cmake --build build
sudo cmake --install build
