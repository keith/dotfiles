index=$(tmux list-panes | grep -oE '\d+\s\(active\)' | awk '{ print $1; }')
__pwd=$(tmux show-environment | grep -e "^TMUXPWD_$index=" | sed 's/.*=//')
if [[ -z $__pwd ]];then
    __pwd=$HOME
fi
tmux $1 $2 -c "$__pwd"
