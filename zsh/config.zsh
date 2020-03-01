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

# Write the running time of commands to the history
# NOTE: the zsh documentation says this and INC_APPEND_HISTORY are mutually
# exclusive, but this seems to work fine.
setopt INC_APPEND_HISTORY_TIME

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

# Clear the RPS1 in the scrollback, since I use this for vim mode indication,
# once you're done editing the text, it's not useful information
setopt TRANSIENT_RPROMPT

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

# Make insert mode allows delete
# https://superuser.com/a/533685
# https://unix.stackexchange.com/a/368576/30131
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^H' backward-delete-char

# You know.
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

# Allow alt/option . to insert the argument from the previous command
bindkey '\e.' insert-last-word

export HELPDIR=/usr/share/zsh/$ZSH_VERSION/help/
# There is a default alias that overrides this function unless it's removed
unalias run-help
autoload -Uz run-help
autoload -Uz run-help-git
bindkey '^o' run-help

# Page up and down since I accidentally hit those sometimes
bindkey '^[[5~' up-line-or-history
bindkey '^[[6~' down-line-or-history

# Enable zmv http://onethingwell.org/post/24608988305/zmv
autoload -U zmv

# Auto insert quotes on typed URLs
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Stop highlighting text when it's pasted. zle_bracketed_paste is a feature to
# not allow multiline pastes while executing each line. I do this all the time
# when I accidentally paste an entire file into my zsh buffer. When this feature
# was added in zsh 5.1, I'm not sure if I didn't notice it, or the highlighting
# options didn't exist, but originally I disabled it all together with:
# unset zle_bracketed_paste
# But now we can just disable the highlighting that adds a lot of visual churn,
# but keep the feature for our own sanity.
zle_highlight=(paste:none)

# Enable bracketed-paste-magic so that url-quote-magic works while bracketed
# paste is enabled. It seems as though the self-insert functionality is only
# allowed with bracketed paste disabled, or with this bracketed-paste-magic
# widget enabled
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# Trim the pasted contents once it's been handled by bracket-paste
# This is for when you paste multiline strings (or strings with trailing
# newlines). It strips any whitespace from the beginning and end of the string,
# and sets the trimmed content in the zle buffer.
zstyle ':bracketed-paste-magic' paste-finish trim-pasted
function trim-pasted() {
  new_content="$PASTED"
  new_content="${new_content#"${new_content%%[![:space:]]*}"}"
  new_content="${new_content%"${new_content##*[![:space:]]}"}"
  PASTED="$new_content"
}
