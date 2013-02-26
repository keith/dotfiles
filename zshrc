PATH="/usr/local/bin:/usr/local/sbin:$HOME/.rbenv/bin:/sbin:/usr/sbin:/usr/local/share/npm/bin:/usr/local/share/python:~/bin:$PATH"

export EDITOR=`which vim`
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
export PKG_CONFIG_PATH="/opt/X11/lib/pkgconfig"

autoload -U colors && colors
setopt prompt_subst

# From Oh-my-zsh
# get the name of the branch we are on
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo "[$(parse_git_dirty)${ref#refs/heads/}]"
}

# Checks if working tree is dirty
parse_git_dirty() {
  local SUBMODULE_SYNTAX='--ignore-submodules=dirty'
    if [[ -n $(git status -s ${SUBMODULE_SYNTAX}  2> /dev/null) ]]; then
      echo "%{$fg[red]%}*%{$reset_color%} "
    fi
}

# Gets the current ruby version from rbenv
function ruby_version() {
	local v=''
	if which rbenv &> /dev/null; then
	  echo "<$(rbenv version | sed -e "s/ (set.*$//")>"
	fi
}


PROMPT='┌ %n@%m %~ $(git_prompt_info) $(ruby_version)
└ %# '

source ~/.aliases

eval "$(rbenv init -)"

HISTSIZE=1000
if (( ! EUID )); then
HISTFILE=~/.zsh_history_root
else
HISTFILE=~/.zsh_history
fi
SAVEHIST=1000

fpath=(/usr/local/share/zsh-completions $fpath)

bindkey -e
# bindkey -v

autoload -Uz compinit
compinit -i
