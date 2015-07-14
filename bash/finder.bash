#!/bin/bash

top_finder_path() {
  osascript 2>/dev/null <<EOF
    tell application "Finder"
      return POSIX path of (target of window 1 as alias)
    end tell
EOF
}

cdf() {
  cd "$(top_finder_path)"
}
