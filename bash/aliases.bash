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

# Remove all items from the dock
alias cleardock="defaults write com.apple.dock persistent-apps -array \"\" && killall Dock"

if ! command -v fd >/dev/null 2>&1 && command -v fdfind >/dev/null 2>&1; then
  alias fd="fdfind --hidden"
fi
alias fd="fd --hidden"
