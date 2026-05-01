I setup a few different scripts for backing up a linux desktop to an
extra drive that's also installed in the case.

- `setup-drive.sh`: wipe a disk, setup mounting and `fstab`
- `backup-status`: print stuff about the last run
- `setup-schedule.sh`: setup the systemd crons for backup
- `rsync-backup.sh`: run the actual backups
