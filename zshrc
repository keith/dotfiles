PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PROMPT="=> "
source ~/Dropbox/Application\ Support/dotfiles/aliases

HISTSIZE=1000
if (( ! EUID )); then
  HISTFILE=~/.history_root
else
  HISTFILE=~/.history
fi
SAVEHIST=1000

autoload -U compinit
compinit -i
