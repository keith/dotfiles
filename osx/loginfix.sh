#!/bin/bash

set -e

file="/usr/local/bin/loginfix.sh"

echo "#!/bin/bash" > "$file"
echo "rm /Users/*/Library/Preferences/ByHost/com.apple.loginwindow.*" >> "$file"
chmod +x "$file"

defaults write com.apple.loginwindow LoginHook "$file"
