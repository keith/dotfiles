# Set prompt to % for users and # for root
export PS1='%# '

fpath=($DOTFILES/functions $fpath)
autoload -Uz $DOTFILES/functions/*(:t)

# Do completions from anywhere in the word
setopt COMPLETE_IN_WORD


# History settings
# Save x items to the given history file
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$HOME/.zsh_history

# Append history to the zsh_history file
setopt APPEND_HISTORY

# Ignore duplicates in zsh history
setopt HIST_IGNORE_ALL_DUPS

# Ignore commands for history that start with a space
setopt HIST_IGNORE_SPACE

# Remove superfluous blanks from each command line being added to the history list
setopt HIST_REDUCE_BLANKS


# Use vim shortcuts within the terminal (defaults to insert mode)
bindkey -v

# Reduce the lag switching into Normal mode to 0.1s
export KEYTIMEOUT=1

# Restore 'normal' search in VI mode
bindkey '^R' history-incremental-search-backward

# Allow alt/option . to insert the argument from the previous command
bindkey '\e.' insert-last-word

