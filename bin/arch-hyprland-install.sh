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
  keyd
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

sudo pacman -S --noconfirm --needed "${hyprland_packages[@]}"

aur_packages=(
  google-chrome
)

yay -S --noconfirm --needed "${aur_packages[@]}"

user=$USER
sudo groupadd -f keyd
sudo usermod -a -G keyd "$user"

sudo install -d -o root -g root -m 0755 /etc/keyd
sudo ln -sfn "$DOTFILES/config/keyd/default.conf" /etc/keyd/default.conf
sudo keyd check /etc/keyd/default.conf
sudo systemctl enable --now keyd
sudo keyd reload

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
