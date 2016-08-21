fpath=($DOTFILES/functions $fpath)
autoload -Uz $DOTFILES/functions/*(:t)

# History settings
# Save x items to the given history file
HISTSIZE=1000000000000000
SAVEHIST=$HISTSIZE
HISTFILE=$HOME/.zsh_history

# Append history to the zsh_history file
setopt APPEND_HISTORY

# Write to history after each command
setopt INC_APPEND_HISTORY

# Don't store the history command
setopt HIST_NO_STORE

# Ignore duplicates in zsh history
setopt HIST_IGNORE_ALL_DUPS

# Ignore commands for history that start with a space
setopt HIST_IGNORE_SPACE

# Remove superfluous blanks from each line being added to the history list
setopt HIST_REDUCE_BLANKS

# After !! previous command don't execute, allow editing
setopt HIST_VERIFY

# pushd for cd commands
setopt AUTO_PUSHD
setopt CDABLE_VARS
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_MINUS
setopt PUSHD_SILENT
setopt PUSHD_TO_HOME
DIRSTACKSIZE=16

# Special chars as file globs
setopt EXTENDED_GLOB

# Jump to end after completion
setopt ALWAYS_TO_END

# Show menu after multiple tabs
setopt AUTO_MENU

# No beep
unsetopt BEEP
unsetopt HIST_BEEP
unsetopt LIST_BEEP

# Warn before quitting with background jobs
setopt CHECK_JOBS

# Don't ask before running a rm *
setopt RM_STAR_SILENT

# Use vim shortcuts within the terminal (defaults to insert mode)
bindkey -v

# Restore 'normal' search in VI mode
# Use C-r to start searches, while in them use C-p and C-n to navigate them
bindkey '^R' history-incremental-search-backward
bindkey -M isearch '^P' history-incremental-search-backward
bindkey -M isearch '^N' history-incremental-search-forward
# Use C-p and C-n to navigate through history while not searching
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

# You can hit C-X to open your $EDITOR
# with the command typed in the buffer
autoload edit-command-line
zle -N edit-command-line
bindkey '^X' edit-command-line
bindkey -M vicmd '^X' edit-command-line

# You know.
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

# Allow alt/option . to insert the argument from the previous command
bindkey '\e.' insert-last-word

# Page up and down since I accidentally hit those sometimes
bindkey '^[[5~' up-line-or-history
bindkey '^[[6~' down-line-or-history

# Auto insert quotes on typed URLs
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Stop highlighting text when it's pasted
# This feature was intended so you wouldn't paste text with a \n and that would
# immediately be interpreted as a "execute command". This feature is totally
# fine but the side effect of giving a background highlight color to the pasted
# text was too much visual churn for me.
# https://github.com/zsh-users/zsh/commit/a2efa4d1c809a098b8fbf3a0cd9cc2e77300bf5a
unset zle_bracketed_paste
