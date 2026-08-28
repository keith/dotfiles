#!/bin/sh

set -euo pipefail

sudo pkg_add \
 "jdk%25" \
 bash \
 cmake \
 fd \
 fzf \
 fzy \
 git \
 go \
 htop \
 lsblk \
 neovim \
 protobuf \
 readline \
 ripgrep \
 sqlite3 \
 unzip \
 vim-- \
 zip \
 zsh
