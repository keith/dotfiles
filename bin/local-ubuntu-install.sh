#!/usr/bin/env bash

set -euo pipefail

# NOTE: This script probably assumes you've already run `ubuntu-default-install.sh`

sudo apt-get install -y \
  avahi-daemon \
  lm-sensors

user=$USER
sudo groupadd -f docker
sudo usermod -a -G docker "$user"

# Setup SysRq key to allow for emergency system commands (e.g., prioritize OOM
# kill, kill all processes, etc.)
echo 'kernel.sysrq=1' | sudo tee /etc/sysctl.d/99-sysrq.conf
sudo sysctl -p /etc/sysctl.d/99-sysrq.conf

# NOTE: Can't have '.' in the file name
sudoers_tmp=$(mktemp)
cat > "$sudoers_tmp" <<'EOF'
Defaults:%sudo env_keep += "EDITOR"
Defaults:%sudo env_keep += "SUDO_EDITOR"
Defaults:%sudo env_keep += "PATH"
Defaults:%sudo !secure_path
EOF
sudo visudo -cf "$sudoers_tmp"
sudo install -o root -g root -m 0440 "$sudoers_tmp" /etc/sudoers.d/env_keep_editor
rm -f "$sudoers_tmp"

# Needed for gpg-agent forwarding
echo 'StreamLocalBindUnlink yes' | sudo tee /etc/ssh/sshd_config.d/99_stream_local_bind_unlink.conf

systemctl restart sshd

cat <<'EOF' | sudo tee /etc/landscape/client.conf
[sysinfo]
exclude_sysinfo_plugins = Temperature
EOF

sudo rm -f /etc/update-motd.d/51-custom-temperature
sudo ln -s "$HOME/.bin/cpu-temp" /etc/update-motd.d/51-custom-temperature
sudo rm -f /etc/update-motd.d/00-header
sudo rm -f /etc/update-motd.d/10-help-text
