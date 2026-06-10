#!/usr/bin/env bash

set -euo pipefail
set -x

hyprland_packages=(
  brightnessctl
  hypridle
  hyprland
  hyprlock
  hyprpolkitagent
  hyprshutdown
  mako
  waybar
  wl-clipboard
)

sudo pacman -S --noconfirm --needed "${hyprland_packages[@]}"

hyprpm update
hyprpm add https://github.com/hyprwm/hyprland-plugins
hyprpm add https://github.com/keith/hyprland-modmove
hyprpm enable hyprbars
hyprpm enable modmove
hyprpm reload -n
