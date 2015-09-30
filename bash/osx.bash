# Make sure we're running OS X
if [[ "$OSX" != true ]];then
  return
fi

# Homebrew cask install apps to global Applications
export HOMEBREW_CASK_OPTS="--appdir=/Applications"

# Disable CocoaPods stats ¯\_(ツ)_/¯
export COCOAPODS_DISABLE_STATS=1

# Open with finder
alias o="open"
alias oo="open ."

# Open MacVim
alias vm="mvim"
alias vmm="mvim ."

# CocoaPods
alias pi="pod install --no-repo-update"
alias psl="pod spec lint *.podspec"
alias psla="pod spec lint *.podspec --verbose"
alias pclean="rm -rf \$HOME/Library/Caches/CocoaPods /tmp/CocoaPods"

# Clean up LaunchServices to remove duplicates in the "Open With" menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias lslist="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -dump"

# Clear the Quarantine database
alias qcleanup="sqlite3 \$HOME/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/\*/.Trashes; sudo rm -rfv \$HOME/.Trash/; sudo rm -rfv /private/var/log/asl/\*.asl"
alias secureemptytrash="sudo srm -rfv /Volumes/\*/.Trashes; sudo srm -rfv \$HOME/.Trash/; sudo srm -rfv /private/var/log/asl/\*.asl"

# Show/hide hidden files in Finder
alias show="defaults write com.apple.Finder AppleShowAllFiles -bool TRUE; killall Finder"
alias hide="defaults write com.apple.Finder AppleShowAllFiles FALSE; killall Finder"

# Hide/show all desktop icons (useful when presenting)
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"

# vim:tw=0
