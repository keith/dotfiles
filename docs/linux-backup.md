I setup a few different scripts for backing up a linux desktop to an
extra drive that's also installed in the case.

- `setup-backup-drive`: wipe a disk, setup mounting and `fstab`
- `backup-status`: print stuff about the last run
- `setup-backup-schedule`: setup the systemd crons for backup
- `rsync-backup`: run the actual backups
