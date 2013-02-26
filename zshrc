PATH="/usr/local/bin:/usr/local/sbin:$HOME/.rbenv/bin:/sbin:/usr/sbin:/usr/local/share/npm/bin:/usr/local/share/python:~/bin:$PATH"

export EDITOR=`which vim`
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig

PS1='%n@%m %~ %#'

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
