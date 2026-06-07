#!/usr/bin/env bash

set -euo pipefail

packages=(
  arp-scan
  autoconf
  base-devel
  bash
  bash-language-server
  clang
  cmake
  colordiff
  ctags
  curl
  fd
  fzf
  fzy
  gdb
  git
  github-cli
  go
  htop
  hyperfine
  jq
  keyd
  less
  lldb
  lm_sensors
  lsof
  lua-language-server
  make
  man-db
  man-pages
  ncdu
  neovim
  networkmanager
  ninja
  nmap
  nodejs
  openbsd-netcat
  parallel
  patchelf
  pre-commit
  protobuf
  pyenv
  python-neovim
  python-pip
  ripgrep
  shellcheck
  speedtest-cli
  strace
  stylua
  sudo
  tmux
  tokei
  tree
  tree-sitter-cli
  ufw
  unzip
  uv
  vim
  wget
  zip
  zsh
)

sudo pacman -S --noconfirm --needed "${packages[@]}"

sudo mkdir -p /etc/NetworkManager/conf.d
cat <<'EOF' | sudo tee /etc/NetworkManager/conf.d/dns.conf
[main]
dns=systemd-resolved
EOF
sudo ln -rsf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf

sudo systemctl disable --now systemd-networkd 2>/dev/null || true
sudo systemctl enable --now NetworkManager systemd-resolved sshd
sudo systemctl restart NetworkManager systemd-resolved sshd

go install github.com/bazelbuild/bazelisk@latest
go install github.com/bazelbuild/buildtools/buildifier@latest
go install github.com/bazelbuild/buildtools/buildozer@latest

user=$USER
sudo groupadd -f keyd
sudo usermod -a -G keyd "$user"

# Setup SysRq key to allow for emergency system commands (e.g., prioritize OOM
# kill, kill all processes, etc.)
echo 'kernel.sysrq=1' | sudo tee /etc/sysctl.d/99-sysrq.conf
sudo sysctl -p /etc/sysctl.d/99-sysrq.conf

sudoers_tmp=$(mktemp)
cat > "$sudoers_tmp" <<'EOF'
%wheel ALL=(ALL:ALL) ALL

Defaults:%sudo env_keep += "EDITOR"
Defaults:%sudo env_keep += "SUDO_EDITOR"
Defaults:%sudo env_keep += "PATH"
Defaults:%sudo !secure_path
EOF
sudo visudo -cf "$sudoers_tmp"
# NOTE: Can't have '.' in the file name
sudo install -o root -g root -m 0440 "$sudoers_tmp" /etc/sudoers.d/env_keep_editor
rm -f "$sudoers_tmp"

# Needed for gpg-agent forwarding
echo 'StreamLocalBindUnlink yes' | sudo tee /etc/ssh/sshd_config.d/99_stream_local_bind_unlink.conf

sudo systemctl restart sshd

sudo install -d -o root -g root -m 0755 /etc/keyd
sudo ln -sfn "$DOTFILES/config/keyd/default.conf" /etc/keyd/default.conf
sudo keyd check /etc/keyd/default.conf
sudo systemctl enable --now keyd
sudo keyd reload

if ! command -v yay 2>/dev/null; then
  rm -rf /tmp/yaysetup
  git clone https://aur.archlinux.org/yay.git /tmp/yaysetup
  pushd /tmp/yaysetup
  makepkg -s
  sudo pacman -U --noconfirm ./*.tar.zst
  popd
fi

aur_packages=(
  ansifilter
  forkstat
  google-chrome
  ufw-docker
)

yay -S "${aur_packages[@]}" --noconfirm --needed

sudo ufw --force reset
sudo ufw default deny incoming
sudo ufw default allow outgoing
sudo ufw limit ssh
sudo ufw allow in proto udp to 224.0.0.251 port 5353 comment 'mDNS IPv4'
sudo ufw allow in proto udp to ff02::fb port 5353 comment 'mDNS IPv6'
sudo ufw --force enable
sudo systemctl enable --now ufw

sudo ufw-docker install
sudo ufw reload
