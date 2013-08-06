# Make sure we're running OS X
if [[ "$OSX" != true ]];then
  return
fi

# Homebrew cask install apps to global Applications
export HOMEBREW_CASK_OPTS="--appdir=/Applications"

# CocoaPods
alias pi="pod install"
alias psla="pod spec lint *.podspec --verbose"
alias pclean="rm -rf $HOME/Library/Caches/CocoaPods /tmp/CocoaPods"

# MacRuby
alias macirb="macirb --simple-prompt"

