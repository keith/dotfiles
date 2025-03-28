# Enable aliases to be sudo'ed
alias sudo="sudo "

# Easier navigation
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

alias G="g"
alias gs="g s"

alias k=kubectl
alias v=nvim
alias bz=bazel

# Remove all items from the dock
alias cleardock="defaults write com.apple.dock persistent-apps -array \"\" && killall Dock"

if ! command -v sha256sum >/dev/null 2>&1; then
  alias sha256sum="shasum -a 256"
fi
