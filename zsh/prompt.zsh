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
zstyle ':vcs_info:*' unstagedstr ' %F{1}M%f'
zstyle ':vcs_info:*' stagedstr ' %F{2}M%f'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats "[%b%u%c%m]"
zstyle ':vcs_info:*' actionformats "[%b%u%c] %F{4}%a%f"
zstyle ':vcs_info:git*+set-message:*' hooks git-st git-stash git-untracked
setopt prompt_subst
export PS1='$base_prompt${vcs_info_msg_0_} %# '
vcs_quiet() {
  vcs_info 2>/dev/null
}
add-zsh-hook precmd vcs_quiet

# Show number of commits ahead or behind of the remote
function +vi-git-st() {
  local ahead behind remote
  local -a gitstatus

  # Are we on a remote-tracking branch?
  remote=${$(git rev-parse --verify ${hook_com[branch]}@{upstream} \
  --symbolic-full-name --abbrev-ref 2>/dev/null)}

  if [[ -n ${remote} ]] ; then
    ahead=$(git rev-list ${hook_com[branch]}@{upstream}..HEAD 2>/dev/null \
    | wc -l | tr -d ' ')

    behind=$(git rev-list HEAD..${hook_com[branch]}@{upstream} 2>/dev/null \
    | wc -l | tr -d ' ')

    diff="${hook_com[branch]}"
    if [[ $ahead -gt 0 ]]; then
      diff="$diff %F{2}$ahead%f"
    fi

    if [[ $behind -gt 0 ]]; then
      diff="$diff%F{1}$behind%f"
    fi

    hook_com[branch]="$diff"
  fi
}

# Show the number of stashes
function +vi-git-stash() {
  local -a stashes

  stashes=$(git stash list 2>/dev/null | wc -l | tr -d ' ')
  if [[ $stashes -gt 0 ]]; then
    hook_com[misc]=" %F{4}${stashes}%f"
  fi
}

# Show a U if there are untracked files
function +vi-git-untracked() {
  untracked=$(git ls-files --other --exclude-standard | wc -l | tr -d ' ')
  if [[ $untracked -gt 0 ]]; then
    hook_com[misc]+="%F{1}?%f"
  fi
}

# Show the hostname over SSH
if [[ -n $SSH_CONNECTION ]];then
  export PS1="%m $PS1"
fi
