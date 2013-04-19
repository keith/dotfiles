# Set the path to include:
#  - /usr/local/bin for Homebrew and others
#  - /usr/local/sbin for Homebrew
#  - $HOME/.rbenv/bin for local rbenv
#  - /usr/local/share/npm/bin import node modules
#  - /usr/local/share/python things installed with pip
PATH="/usr/local/bin:/usr/local/sbin:$HOME/.rbenv/bin:/usr/local/share/npm/bin:/usr/local/share/python:$PATH"

# Set my default editor to Vim :)
export EDITOR=`which vim`

# Set prompt to % for users and # for root
PS1='%# '

# Get zsh aliases
source $HOME/.aliases

# Load rbenv on launch
eval "$(rbenv init -)"

# Append history to the zsh_history file
setopt APPEND_HISTORY

# Ignore duplicates in zsh history
setopt HIST_IGNORE_ALL_DUPS

# Ignore commands for history that start with a space
setopt HIST_IGNORE_SPACE

# Save x items to the given history file
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$HOME/.zsh_history

# Link to zsh-completions files
fpath=(/usr/local/share/zsh-completions $fpath)

# Use vim shortcuts within the terminal (defaults to insert mode)
bindkey -v

# Restore 'normal' search in VI mode
bindkey '^R' history-incremental-search-backward

# Load autocomplete and other zsh stuff
autoload -Uz compinit
compinit -i

