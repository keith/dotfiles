#!/usr/bin/env bash

sudo -v

file=/usr/bin/loginfix.sh

echo "#!/bin/bash" | sudo tee $file > /dev/null
echo "rm /Users/*/Library/Preferences/ByHost/com.apple.loginwindow.*" \
    | sudo tee -a $file > /dev/null
sudo chmod +x $file
sudo chown root:wheel $file

defaults write com.apple.loginwindow LoginHook $file
