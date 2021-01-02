#!/bin/bash

set -euo pipefail

pkgs=(
  nixpkgs.coreutils-full
  nixpkgs.findutils
  nixpkgs.gnumake
  nixpkgs.gnused
  nixpkgs.gnutar
)

nix-env -iA "${pkgs[@]}"
