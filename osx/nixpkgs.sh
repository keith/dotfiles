#!/bin/bash

set -euo pipefail

# TODO: Switch to these
# nixpkgs.git-zsh-completion
# nixpkgs.gitAndTools.gitFull
# nixpkgs.gnupg

readonly pkgs=(
  ansifilter
  arp-scan
  atuin
  chruby
  cmake
  cmake-format
  colordiff
  ctags
  diff-grep
  dockutil
  dylibtree
  fd
  gh
  graphviz
  htop
  hyperfine
  jq
  kubectl
  kubectx
  less
  lua-language-server
  msmtp
  mutt
  ninja
  nixfmt
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
  vim
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
