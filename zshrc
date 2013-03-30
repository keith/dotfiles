PATH="/usr/local/bin:/usr/local/sbin:$HOME/.rbenv/bin:/sbin:/usr/sbin:/usr/local/share/npm/bin:/usr/local/share/python:$HOME/bin:$PATH"

export EDITOR=`which vim`
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
export PKG_CONFIG_PATH="/opt/X11/lib/pkgconfig"

PS1='%# '

source $HOME/.aliases

eval "$(rbenv init -)"

setopt APPEND_HISTORY # adds history
setopt HIST_IGNORE_ALL_DUPS  # don't record dupes in history
setopt HIST_IGNORE_SPACE

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$HOME/.zsh_history

fpath=(/usr/local/share/zsh-completions $fpath)

# bindkey -e
bindkey -v

autoload -Uz compinit
compinit -i
