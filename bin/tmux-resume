index=$(tmux display -p "TMUXPWD_#D" | tr -d %)
__pwd=$(tmux show-environment | grep --color=never "$index" | sed 's/^.*=//')
if [[ -z "$__pwd" || ! -d "$__pwd" ]];then
    __pwd=$HOME
fi
tmux $1 $2 -c "$__pwd"
