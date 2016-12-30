#!/bin/sh
#
# Open the archives for a mailing list at the week containing the piped message
#
# Mutt usage:
#
#     macro pager ,a \
#       "<enter-command>set my_old_pipe_decode=\$pipe_decode nopipe_decode<enter> \
#       <pipe-message>$DOTFILES/mutt/listarchive.sh<enter> \
#       <enter-command>set pipe_decode=\$my_old_pipe_decode<enter> \
#       <top>" \
#       "Open the archive for a mailing list"
#

set -e

message="$(cat)"
archive="$(echo "$message" | grep -i "^list-archive:" | grep -o "<.*>" | tr -d "<>")"

if [ -z "$archive" ]; then
  exit
fi

date=$(echo "$message" | grep -i "^Date: " | sed "s/^Date: //")
if [ -z "$date" ]; then
  open "$archive"
  exit
fi

if date --version > /dev/null 2>&1; then
  # Adjust the date by the number of days to the monday before
  # -1 because Monday=1
  difference=$(date --date "$date -1 day" "+%w")
  list_date=$(date --date "$date -$difference days" "+%Y%m%d")
else
  list_date=$(date -j -v-mon -f "%a, %d %b %Y %H:%M:%S %z" "$date" "+%Y%m%d")
fi
# Open the date view, switch to different HTML for different format
open "$archive/Week-of-Mon-$list_date/date.html"
