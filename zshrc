PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/share/python:~/bin:$PATH"

# export NARWHAL_ENGINE=jsc
# export PATH="/usr/local/narwhal/bin:$PATH"
# export CAPP_BUILD="$HOME/Documents/sources/Cappuccino/Build"
export HOMEBREW_CASK_OPTS='--appdir=/Applications'
export EDITOR=`which vim`

PROMPT="=> "
source ~/Dropbox/Application\ Support/dotfiles/aliases

eval "$(rbenv init -)"

HISTSIZE=1000
if (( ! EUID )); then
HISTFILE=~/.zsh_history_root
else
HISTFILE=~/.zsh_history
fi
SAVEHIST=1000

set -o emacs
# set -o vi

autoload -U compinit
compinit -i
