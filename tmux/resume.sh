active=$(tmux list-panes | grep -oE '\d+\s\(active\)')
index=${active:0:1}
__pwd=$(tmux show-environment | grep -e "^TMUXPWD_$index=" | sed 's/.*=//')
tmux $1 $2 -c "$__pwd"
