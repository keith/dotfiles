#!/bin/bash

set -euo pipefail

pkgs=(
  nixpkgs.bash
  nixpkgs.chruby
  nixpkgs.cmake
  nixpkgs.colordiff
  nixpkgs.coreutils-full
  nixpkgs.ctags
  nixpkgs.curlFull
  nixpkgs.diffutils
  nixpkgs.fd
  nixpkgs.findutils
  nixpkgs.fzy
  nixpkgs.gawk
  nixpkgs.git-zsh-completion
  nixpkgs.gitAndTools.gitFull
  nixpkgs.gnugrep
  nixpkgs.gnumake
  nixpkgs.gnupg
  nixpkgs.gnused
  nixpkgs.gnutar
  nixpkgs.gzip
  nixpkgs.htop
  nixpkgs.ideviceinstaller
  nixpkgs.less
  nixpkgs.msmtp
  nixpkgs.mu
  nixpkgs.mutt
  nixpkgs.neovim
  nixpkgs.newsboat
  nixpkgs.ninja
  nixpkgs.parallel
  nixpkgs.patch
  nixpkgs.reattach-to-user-namespace
  nixpkgs.ripgrep
  nixpkgs.shellcheck
  nixpkgs.tmux
  nixpkgs.tokei
  nixpkgs.tree
  nixpkgs.unixtools.watch
  nixpkgs.vim
  nixpkgs.w3m
  nixpkgs.wget
  nixpkgs.youtube-dl
  nixpkgs.zsh
  nixpkgs.zsh-completions
)

nix-env -iA "${pkgs[@]}"
