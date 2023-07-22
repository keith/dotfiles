#!/bin/bash

set -euo pipefail

# TODO: Switch to these
# nixpkgs.git-zsh-completion
# nixpkgs.gitAndTools.gitFull
# nixpkgs.gnupg
# nixpkgs.vim

readonly pkgs=(
  ansifilter
  arp-scan
  chruby
  cmake
  colordiff
  ctags
  dockutil
  fd
  graphviz
  htop
  hyperfine
  ideviceinstaller
  jq
  kubectl
  kubectx
  less
  msmtp
  mu
  mutt
  ninja
  nmap
  parallel
  pre-commit
  pstree
  radare2
  reattach-to-user-namespace
  ripgrep
  sccache
  shellcheck
  tmux
  tokei
  tree
  unixtools.watch
  w3m
  wget
  youtube-dl
  zsh
  zsh-completions
)

names=()
for pkg in "${pkgs[@]}"; do
  names+=("nixpkgs.$pkg")
done

nix-env -iA "${names[@]}"
