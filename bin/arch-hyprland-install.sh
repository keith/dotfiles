#!/usr/bin/env bash

set -euo pipefail

hyprland_packages=(
  brightnessctl
  flatpak
  greetd-tuigreet # login screen
  grim # screenshots
  hypridle
  hyprland
  hyprlock
  hyprpolkitagent
  hyprshutdown
  libnotify # notify-send
  loupe # image viewer
  pipewire # audio
  pipewire-alsa # audio
  pipewire-audio # audio
  pipewire-pulse # audio
  slurp # screenshots
  sound-theme-freedesktop # Sounds for testing volume changes
  wireplumber # audio
  wl-clipboard
  xdg-desktop-portal-gtk
  xdg-desktop-portal-hyprland
)

set -x
sudo pacman -S --noconfirm --needed "${hyprland_packages[@]}"
yay -S --noconfirm --needed wayle-bin

cat <<'EOF' | sudo tee /etc/greetd/config.toml
[terminal]
vt = 1

[default_session]
command = "tuigreet --time --remember --remember-session --asterisks --greeting 'Arch Linux' --cmd start-hyprland"
user = "greeter"
EOF

sudo systemctl enable greetd.service

systemctl --user enable --now pipewire pipewire-pulse wireplumber

hyprpm update
hyprpm add https://github.com/hyprwm/hyprland-plugins
hyprpm add https://github.com/keith/hyprland-modmove
hyprpm enable hyprbars
hyprpm enable modmove
hyprpm reload -n

flatpak install flathub io.missioncenter.MissionCenter
