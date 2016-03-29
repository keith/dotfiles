#!/bin/sh
#
# Open the archives for a mailing list at the week containing the piped message
#
# Mutt usage:
#
#     macro pager ,b \
#       "<enter-command>set my_old_pipe_decode=\$pipe_decode nopipe_decode<enter> \
#       <pipe-message>$DOTFILES/mutt/swiftbugs.sh<enter> \
#       <enter-command>set pipe_decode=\$my_old_pipe_decode<enter> \
#       <top>" \
#       "View all Swift bug links in urlview"
#

set -e
set -o pipefail

cat \
  | sed -E "s=[[:space:]]([Ss][Rr]-[[:digit:]])= https://bugs.swift.org/browse/\1=g" \
  | grep "bugs.swift.org" \
  | urlview
