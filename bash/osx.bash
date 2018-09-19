# Make sure we're running OS X
if [[ "$OSX" != true ]]; then
  return
fi

# Disable fastlane stuff ¯\_(ツ)_/¯
export FASTLANE_OPT_OUT_USAGE=1
export FASTLANE_SKIP_UPDATE_CHECK=1

# Disable CocoaPods stats ¯\_(ツ)_/¯
export COCOAPODS_DISABLE_STATS=1

# Disable homebrew analytics
export HOMEBREW_NO_ANALYTICS=1
# Disable crazy characters in brew
export HOMEBREW_NO_EMOJI=1
# Stop homebrew from auto-updating
export HOMEBREW_NO_AUTO_UPDATE=1

# Open with finder
alias o="open"
alias oo="open ."

# CocoaPods
alias psl="pod spec lint *.podspec"
alias psla="pod spec lint *.podspec --verbose"

# Clean up LaunchServices to remove duplicates in the "Open With" menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias lslist="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -dump"

# Clear the Quarantine database
alias qcleanup="sqlite3 \$HOME/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/\*/.Trashes; sudo rm -rfv \$HOME/.Trash/; sudo rm -rfv /private/var/log/asl/\*.asl"
alias secureemptytrash="sudo srm -rfv /Volumes/\*/.Trashes; sudo srm -rfv \$HOME/.Trash/; sudo srm -rfv /private/var/log/asl/\*.asl"

# Hide/show all desktop icons (useful when presenting)
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"

alias hopper=hopperv4

# Xcode commands which are not linked
alias simctl="xcrun simctl"

# lldb doesn't work with brewed python
# https://github.com/Homebrew/homebrew-core/issues/2730
# https://github.com/Homebrew/legacy-homebrew/issues/47201
alias lldb='PATH="/usr/bin" lldb'
alias swift='PATH="/usr/bin" swift'

# vim:tw=0
