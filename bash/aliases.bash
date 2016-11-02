# Enable aliases to be sudo'ed
alias sudo="sudo "

# Easier navigation
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# Git
alias g="git"
alias G="git"

# Vim
alias v="vim"
alias vv="vim ."

# Archives
alias mktar="tar -pvczf"
alias untar="tar -zxvf"

# Remove all items from the dock
alias cleardock="defaults write com.apple.dock persistent-apps -array \"\" && killall Dock"
