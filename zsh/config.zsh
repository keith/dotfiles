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

# Restore 'normal' search in VI mode
bindkey '^R' history-incremental-search-backward
bindkey '^P' up-history
bindkey '^N' down-history

# Allow alt/option . to insert the argument from the previous command
bindkey '\e.' insert-last-word

# Show vim mode on right
# http://dougblack.io/words/zsh-vi-mode.html
function zle-line-init zle-keymap-select {
  VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
  RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
  zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

# Reduce the lag switching into Normal mode to 0.1s
export KEYTIMEOUT=1

# Force update of RPS1 immediately
RPS1=""

