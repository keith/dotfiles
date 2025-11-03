#!/bin/bash

set -euo pipefail

sudo apt-get update
sudo apt-get install -y ca-certificates gpg wget

curl -s https://repos.azul.com/azul-repo.key | sudo gpg --dearmor -o /usr/share/keyrings/azul.gpg
echo "deb [signed-by=/usr/share/keyrings/azul.gpg] https://repos.azul.com/zulu/deb stable main" | sudo tee /etc/apt/sources.list.d/zulu.list

wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null \
  | gpg --dearmor - | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null

echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ $(lsb_release -cs) main" \
  | sudo tee /etc/apt/sources.list.d/kitware.list >/dev/null

# https://github.com/cli/cli/blob/trunk/docs/install_linux.md
out=$(mktemp) && wget -nv -O"$out" https://cli.github.com/packages/githubcli-archive-keyring.gpg \
  && cat "$out" | sudo tee /etc/apt/keyrings/githubcli-archive-keyring.gpg > /dev/null \
  && sudo chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg \
  && echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null

sudo apt-add-repository -y ppa:git-core/ppa
sudo add-apt-repository -y ppa:longsleep/golang-backports

sudo apt-get update

sudo apt install -y \
  ansifilter \
  autoconf \
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
  libbz2-dev \
  libedit-dev \
  liblzma-dev \
  libreadline-dev \
  libsqlite3-dev \
  libssl-dev \
  lsof \
  manpages \
  manpages-dev \
  ncurses-term \
  netcat \
  ninja-build \
  parallel \
  patchelf \
  pax-utils \
  protobuf-compiler \
  python3 \
  python3-pip \
  python3-venv \
  ripgrep \
  speedtest-cli \
  strace \
  sudo \
  systemd-coredump \
  tmux \
  tree \
  universal-ctags \
  unzip \
  urlview \
  vim \
  zip \
  zsh \
  zstd \
  zulu24-jdk

sudo rm -f /usr/share/keyrings/kitware-archive-keyring.gpg
sudo apt autoremove -y

go install github.com/bazelbuild/bazelisk@latest
go install github.com/bazelbuild/buildtools/buildifier@latest
go install github.com/bazelbuild/buildtools/buildozer@latest

pip3 install cmake-language-server
pip3 install neovim
pip3 install pyright

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
"$script_dir"/install-nvim

cd /tmp
git clone https://github.com/keith/tag
cd tag
cmake -B build
cmake --build build
sudo cmake --install build
