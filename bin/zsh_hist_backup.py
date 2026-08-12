#!/usr/bin/env python3

from pathlib import Path
import os
import time
import shutil

# Cron example:
#
# */5 * * * * /path/to/zsh_hist_backup.py
#

# - Create a backup from the current zsh history file
# - Read all backup history files and create a merged copy using the order from the largest file for duplicate commands
# - Remove all except some of the newest and largest files

# This script is attempting to make sure I never lose some command history even if I don't notice right away, theoretically the merged history file should always have all combined history

_BACKUP_DIR = Path("~/zsh_backups").expanduser()
_BACKUP_DIR.mkdir(exist_ok=True)

backup_file = _BACKUP_DIR / f"history_{int(time.time())}"
shutil.copyfile(Path("~/.keith_zsh_history").expanduser(), backup_file)

# Remove all except the longest N files in the backup directory
history_files = set(_BACKUP_DIR.glob("*"))
largest_files = sorted(history_files, key=os.path.getsize)[-5:]
newest_files = sorted(history_files, key=os.path.getctime)[-5:]

largest_file = largest_files[-1]
with open(largest_file, "rb") as f:
    longest_file_lines = [
        line for raw_line in f.read().splitlines() if (line := raw_line.lstrip())
    ]
    set_for_deduping = set(longest_file_lines)

new_lines = []
for line in longest_file_lines:
    if line in set_for_deduping:
        set_for_deduping.remove(line)
        new_lines.append(line)

all_other_lines = set()
for history_file in history_files - {largest_file}:
    with open(history_file, "rb") as f:
        for line in f.read().splitlines():
            if line := line.lstrip():
                all_other_lines.add(line)

new_lines = list(all_other_lines - set(new_lines)) + new_lines

merged_file = Path("~/zsh_merged_backup").expanduser()
with open(merged_file, "wb") as f:
    f.write(b"\n".join(new_lines))
    f.write(b"\n")

stale_files = history_files - set(largest_files) - set(newest_files) - {backup_file}
for stale_file in stale_files:
    stale_file.unlink()
