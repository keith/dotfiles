PATH="/usr/local/bin:/usr/local/sbin:~/bin:$PATH"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
PATH=$PATH:/usr/local/share/npm/bin
PATH=$PATH:/usr/local/sbin


PROMPT="=> "
source ~/Dropbox/Application\ Support/dotfiles/aliases

HISTSIZE=1000
if (( ! EUID )); then
  HISTFILE=~/.zsh_history_root
else
  HISTFILE=~/.zsh_history
fi
SAVEHIST=1000

autoload -U compinit
compinit -i
