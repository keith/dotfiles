# Show 2 top $PWD components
# Show bg jobs >= 1 in yellow
# % for users and # for root
autoload -U colors && colors
base_prompt="(%2c%{$fg[yellow]%}%(1j. %j.)%{$reset_color%})"
export PS1="$base_prompt %# "

# Not sold on Git info in your prompt, but if I used it
#  it would probably look a lot like this:
autoload -U add-zsh-hook
autoload -Uz vcs_info
# # Colors:
# # 9: Orange
# # 6: Teal
# # 5: Pink
# # 4: Blue
# # 3: Yellow
# # 2: Green
# # 1: Red
# # 0: Black
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' unstagedstr '%F{1}M%f'
zstyle ':vcs_info:*' stagedstr '%F{2}M%f'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats "[%b%u%c]"
zstyle ':vcs_info:*' actionformats "(%F{2}%b%F{1}%u%F{2}%c%f) %F{4}%a%f"
# zstyle ':vcs_info:git*+set-message:*' hooks git-st
setopt prompt_subst
export PS1='$base_prompt${vcs_info_msg_0_} %# '
add-zsh-hook precmd vcs_info

# Show remote ref name and number of commits ahead-of or behind
# function +vi-git-st() {
#   local ahead behind remote
#   local -a gitstatus

#   # Are we on a remote-tracking branch?
#   remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
#   --symbolic-full-name --abbrev-ref 2>/dev/null)}

#   if [[ -n ${remote} ]] ; then
#     ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null \
#     | wc -l | tr -d ' ')
#     # (( $ahead )) && gitstatus+=( "${green}+${ahead}${gray}" )
#     # (($ahead))

#     behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null \
#     | wc -l | tr -d ' ')
#     # (( $behind )) && gitstatus+=( "${red}-${behind}${gray}" )

#     # user_data[gitstatus]=${gitstatus}
#     if [[ $ahead -gt 1 || $behind -gt 1 ]]; then
#       hook_com[branch]="${hook_com[branch]}[$ahead:$behind]"
#     fi
#   fi
# }

# Show the hostname over SSH
if [[ -n $SSH_CLIENT ]];then
  export PS1="%m $PS1"
fi

fpath=($DOTFILES/scripts/zsh-completions/src $fpath)
fpath=($DOTFILES/functions $fpath)
autoload -Uz $DOTFILES/functions/*(:t)

# Do completions from anywhere in the word
setopt COMPLETE_IN_WORD


# History settings
# Save x items to the given history file
HISTSIZE=5000
SAVEHIST=5000
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

# pushd for cd commands
setopt AUTO_PUSHD
# Don't push duplicates
setopt PUSHD_IGNORE_DUPS

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
add-zsh-hook precmd reset_rps1
