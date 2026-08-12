if [[ -n "$TMUX" ]]; then
  # Record this pane's cwd for tmux-resume to open new panes in the same directory.
  _tmux_record_pwd() {
    tmux setenv "TMUXPWD_${TMUX_PANE#\%}" "$PWD"
  }
  chpwd_functions=($chpwd_functions _tmux_record_pwd)
  # chpwd only fires on directory changes, record the starting directory too
  _tmux_record_pwd
fi
