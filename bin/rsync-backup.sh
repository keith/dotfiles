#!/usr/bin/env bash
#
# Snapshot-style backup using rsync --link-dest.
# Each run creates a timestamped directory; unchanged files are hardlinked
# to the previous snapshot, so old snapshots cost almost no extra space.
#
# Usage:
#   rsync-backup.sh              # normal run
#   rsync-backup.sh --dry-run    # show what would change, write nothing
#

set -euo pipefail

readonly backup_mount="/mnt/backup"
readonly backup_dir="$backup_mount/snapshots"
readonly retention=168 # snapshots to keep (~10 days at this schedule)
readonly status_dir="/var/lib/backup"
readonly status_file="$status_dir/status"

readonly latest="$backup_dir/latest"
new="$backup_dir/$(date +%Y-%m-%d_%H%M%S)"

dry=()
dry_run=0
if [[ "${1:-}" == "--dry-run" ]]; then
  dry=(--dry-run --itemize-changes)
  dry_run=1
fi

mkdir -p "$status_dir"

log_status() {
  local result="$1"
  local message="$2"
  cat > "$status_file" <<EOF
last_run=$(date -Iseconds)
last_result=$result
last_message=$message
EOF
}

if ! mountpoint -q "$backup_mount"; then
  log_status "FAIL" "Backup drive not mounted at $backup_mount"
  echo "Backup drive not mounted at $backup_mount" >&2
  exit 1
fi

rsync_opts=(
  -aAXH
  --delete

  # System paths
  --exclude=/dev/*
  --exclude=/lost+found
  --exclude=/media/*
  --exclude=/mnt/*
  --exclude=/proc/*
  --exclude=/run/*
  --exclude=/swapfile
  --exclude=/sys/*
  --exclude=/tmp/*
  --exclude=/var/cache/*
  --exclude=/var/tmp/*

  # User caches and trash
  --exclude=/home/*/.cache/*
  --exclude=/home/*/.local/share/Trash/*

  # Build / language toolchain caches — all regenerable, can be huge
  --exclude=/home/*/.cargo/*
  --exclude=/home/*/.ccache/*
  --exclude=/home/*/.gradle/*
  --exclude=/home/*/.rustup/*
  --exclude=/home/*/go/pkg/*

  # Build output anywhere on disk
  --exclude=*/build/
  --exclude=*/target/
  --exclude=*/node_modules/
)

# Hardlink unchanged files to the previous snapshot
if [[ -d "$latest" ]]; then
  rsync_opts+=(--link-dest="$latest")
fi

if ionice -c 3 nice -n 19 rsync "${rsync_opts[@]}" "${dry[@]}" / "$new/"; then
  if [[ $dry_run -eq 1 ]]; then
    rm -rf -- "$new" 2>/dev/null || true
    echo "Dry run complete (no changes made)."
    exit 0
  fi

  # Update the 'latest' symlink
  ln -sfn "$new" "$latest"

  # Prune oldest snapshots beyond retention count
  cd "$backup_dir"
  # shellcheck disable=SC2012
  ls -1d 20*-*-*_* 2>/dev/null | sort -r | tail -n +$((retention + 1)) | while read -r old; do
    rm -rf -- "$old"
  done

  log_status "OK" "Snapshot at $new"
  echo "Backup complete: $new"
else
  log_status "FAIL" "rsync exited non-zero for $new"
  echo "rsync failed" >&2
  exit 1
fi
