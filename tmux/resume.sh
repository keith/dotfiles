window=$(tmux list-windows | grep active | awk '{ print $1; }' | sed 's/://')
pane=$(tmux list-panes -t $window | grep active | awk '{ print $7; }' | sed 's/%//')
__pwd=$(tmux show-environment | grep -e "^TMUXPWD_$pane=" | sed 's/.*=//')
tmux $1 $2 -c "$__pwd"
