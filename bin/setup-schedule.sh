#!/usr/bin/env bash
#
# Create a systemd timer that runs rsync-backup.sh hourly outside of work hours,
# and surface backup status on SSH login via MOTD.
#

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
readonly backup_script_path="$script_dir/rsync-backup.sh"
readonly status_script_path="$script_dir/backup-status"
readonly unit_name="rsync-backup"
readonly motd_path="/etc/update-motd.d/99-backup-status"

if [[ $EUID -ne 0 ]]; then
  echo "error: run with sudo" >&2
  exit 1
fi

if [[ ! -x "$backup_script_path" ]]; then
  echo "error: rsync-backup.sh not found, expected: $backup_script_path" >&2
  exit 1
fi

if [[ ! -x "$status_script_path" ]]; then
  echo "error: backup-status not found, expected: $status_script_path" >&2
  exit 1
fi

ln -s "$status_script_path" "$motd_path"

cat > "/etc/systemd/system/$unit_name.service" <<EOF
[Unit]
Description=Snapshot backup via rsync
RequiresMountsFor=/mnt/backup

[Service]
Type=oneshot
ExecStart=$backup_script_path
Nice=19
IOSchedulingClass=idle
EOF

cat > "/etc/systemd/system/$unit_name.timer" <<EOF
[Unit]
Description=Hourly backup outside of work hours

[Timer]
# Weekdays: evenings (6pm-11pm) and overnight/morning (midnight-8am)
OnCalendar=Mon..Fri *-*-* 18..23:00:00
OnCalendar=Mon..Fri *-*-* 00..08:00:00
# Weekends: every hour
OnCalendar=Sat,Sun *-*-* *:00:00
# Persistent=false (default): missed runs while powered off are NOT made up.
# This prevents a backup from kicking off right when you boot up to use the machine.
RandomizedDelaySec=300

[Install]
WantedBy=timers.target
EOF

systemctl daemon-reload
systemctl enable --now "$unit_name.timer"

cat <<EOF
Schedule setup complete.

  Backup script:   $backup_script_path
  Status script:   $status_script_path
  Schedule:        hourly, except 9am-6pm Mon-Fri (no boot catch-up)
  Status banner:   $motd_path (shows on SSH login)

Note: scripts are run from where they live in your dotfiles. If you move them,
re-run this setup script to update the systemd unit and MOTD hook.

Useful commands:
  Run a backup now:        sudo systemctl start $unit_name.service
  Test without writing:    sudo $backup_script_path --dry-run
  See timer schedule:      systemctl list-timers $unit_name.timer
  View backup logs:        journalctl -u $unit_name.service
  Show status manually:    $status_script_path
EOF
