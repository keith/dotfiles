PATH="/usr/local/bin:/usr/local/sbin:/home/ksmiley/.rbenv/bin:/sbin:/usr/sbin:/usr/local/share/npm/bin:/usr/local/share/python:~/bin:$PATH"

export EDITOR=`which vim`
export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"

PS1="~> "

source ~/.aliases

eval "$(rbenv init -)"

HISTSIZE=1000
if (( ! EUID )); then
HISTFILE=~/.zsh_history_root
else
HISTFILE=~/.zsh_history
fi
SAVEHIST=1000

bindkey -e
# bindkey -v

autoload -Uz compinit
compinit -i

