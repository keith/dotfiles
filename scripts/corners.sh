#!/usr/bin/env bash

function die () {
    echo "Usage ./$(basename $0) {enable|disable}"
    exit
}

if [[ $# != 1 ]]; then
    die
fi

# Hot corners
# Possible values:
#  0: no-op
#  2: Mission Control
#  3: Show application windows
#  4: Desktop
#  5: Start screen saver
#  6: Disable screen saver
#  7: Dashboard
# 10: Put display to sleep
# 11: Launchpad
# 12: Notification Center

if [[ $1 == "enable" ]]; then
    # Bottom left screen corner
    defaults write com.apple.dock wvous-bl-corner -int 2
    defaults write com.apple.dock wvous-bl-modifier -int 0
    # Bottom right screen corner
    defaults write com.apple.dock wvous-br-corner -int 4
    defaults write com.apple.dock wvous-br-modifier -int 0
    # Top left screen corner
    defaults write com.apple.dock wvous-tl-corner -int 10
    defaults write com.apple.dock wvous-tl-modifier -int 0
    # Top right screen corner
    defaults write com.apple.dock wvous-tr-corner -int 12
    defaults write com.apple.dock wvous-tr-modifier -int 0

    killall Dock
elif [[ $1 == "disable" ]]; then
    # Bottom left screen corner
    defaults write com.apple.dock wvous-bl-corner -int 0
    defaults write com.apple.dock wvous-bl-modifier -int 0
    # Bottom right screen corner
    defaults write com.apple.dock wvous-br-corner -int 0
    defaults write com.apple.dock wvous-br-modifier -int 0
    # Top left screen corner
    defaults write com.apple.dock wvous-tl-corner -int 0
    defaults write com.apple.dock wvous-tl-modifier -int 0
    # Top right screen corner
    defaults write com.apple.dock wvous-tr-corner -int 0
    defaults write com.apple.dock wvous-tr-modifier -int 0

    killall Dock
else
    die
fi

