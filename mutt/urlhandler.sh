#!/bin/sh

# Because Chrome
frontmost() {
osascript <<EOF
tell application "System Events"
  set frontApp to name of first application process whose frontmost is true
end tell
EOF
}

before=$(frontmost)
open -g "$1"
sleep 0.1
after=$(frontmost)

if [ "$before" != "$after" ]; then
  open -a "$before"
fi
