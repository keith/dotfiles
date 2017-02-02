autoload -U add-zsh-hook
autoload -U colors && colors
autoload -Uz vcs_info
setopt prompt_subst
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

# Show number of commits ahead or behind of the remote
function +vi-git-st() {
  local ahead behind remote

  # Are we on a remote-tracking branch?
  remote=${$(git rev-parse --verify ${hook_com[branch]}"@{upstream}" \
  --symbolic-full-name --abbrev-ref 2>/dev/null)}

  if [[ -n ${remote} ]]; then
    ahead=$(git rev-list ${hook_com[branch]}"@{upstream}"..HEAD 2>/dev/null \
      | wc -l | tr -d ' ')

    behind=$(git rev-list HEAD..${hook_com[branch]}"@{upstream}" 2>/dev/null \
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

function RCMD() {
  vcs_info 2>/dev/null
  echo "${vcs_info_msg_0_}"
}

function setup-prompt() {
  git_info=$1
  ssh=""
  venv=""
  venv_padding=""

  # Show the hostname over SSH
  if [[ -n $SSH_CONNECTION ]]; then
    ssh="%m "
  fi

  if [[ -n $VIRTUAL_ENV ]]; then
    venv="v"
    venv_padding=" "
  fi

  NEW_PROMPT=""
  # The local hostname if the current session is over ssh
  NEW_PROMPT="$NEW_PROMPT$ssh"
  # Opening paren and 2 components of pwd
  NEW_PROMPT="$NEW_PROMPT(%2c"
  # Yellow number of jobs, if there are any jobs, otherwise empty string
  NEW_PROMPT="$NEW_PROMPT%{$fg[yellow]%}%(1j. %j.)%{$reset_color%}"
  # Green virtualenv info, prefix with space (unless it's already done by jobs)
  NEW_PROMPT="$NEW_PROMPT%{$fg[green]%}%(1j..$venv_padding)$venv%{$reset_color%}"
  # Closing paren around pwd, any passed git information
  NEW_PROMPT="$NEW_PROMPT)$git_info"
  # Either green/red (based on previous command exit code) either %/# (depending on root)
  NEW_PROMPT="$NEW_PROMPT %(?.%{$fg[green]%}%#%{$reset_color%} .%{$fg[red]%}%#%{$reset_color%} )"

  PROMPT="$NEW_PROMPT"
}
setup-prompt ""

# http://www.anishathalye.com/2015/02/07/an-asynchronous-shell-prompt/
# https://github.com/anishathalye/dotfiles
ASYNC_PROC=0
function right-prompt() {
  function async() {
    # save to temp file
    printf "%s" "$(RCMD)" > "$HOME/.zsh_tmp_prompt"

    # signal parent
    kill -s USR1 $$
  }

  # kill child if necessary
  if [[ "${ASYNC_PROC}" != 0 ]]; then
    kill -s HUP $ASYNC_PROC >/dev/null 2>&1 || :
  fi

  # start background computation
  async &!
  ASYNC_PROC=$!
}
add-zsh-hook precmd right-prompt

function TRAPUSR1() {
  # read from temp file
  setup-prompt "$(cat $HOME/.zsh_tmp_prompt)"

  # reset proc number
  ASYNC_PROC=0

  # redisplay
  zle && zle reset-prompt
}

# Right prompt + vim mode

# Reduce the lag switching into Normal mode to 0.1s
export KEYTIMEOUT=1

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

# Force update of RPS1 immediately
reset_rps1() {
  RPS1=""
}
add-zsh-hook precmd reset_rps1
