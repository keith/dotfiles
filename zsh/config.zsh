# Set prompt to % for users and # for root
# export PS1="%# "

# Show 2 top $PWD components
# Show bg jobs >= 1 in yellow
# % for users and # for root
autoload -U colors && colors
export PS1="(%2c%{$fg[yellow]%}%(1j. %j.)%{$reset_color%}) %# "

# Not sold on Git info in your prompt, but if I used it
#  it would probably look a lot like this:
# autoload -Uz vcs_info
# # Colors:
# # 9: Orange
# # 6: Teal
# # 5: Pink
# # 4: Blue
# # 3: Yellow
# # 2: Green
# # 1: Red
# # 0: Black
# zstyle ':vcs_info:*' unstagedstr '!'
# zstyle ':vcs_info:*' stagedstr '?'
# zstyle ':vcs_info:*' check-for-changes true
# zstyle ':vcs_info:*' formats "(%F{2}%b%F{1}%u%F{2}%c%f)"
# zstyle ':vcs_info:*' actionformats "(%F{2}%b%F{1}%u%F{2}%c%f) %F{4}%a%f"
# zstyle ':vcs_info:*' enable git
# setopt prompt_subst
# export PS1='${vcs_info_msg_0_} %# '
# precmd_functions=($precmd_functions vcs_info)

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
  VIM_PROMPT="[% NORMAL]%"
  # Apparently EPS1 is not a typo
  RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
  zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

# Reduce the lag switching into Normal mode to 0.1s
export KEYTIMEOUT=1

# Force update of RPS1 immediately
reset_rps1() {
  RPS1=""
}
precmd_functions=($precmd_functions reset_rps1)
