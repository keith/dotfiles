__fsel() {
  command find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | sed 1d | cut -b3- | $(__fzfcmd) -m | while read item; do
    printf '%q ' "$item"
  done
  echo
}

__fzfcmd() {
  [ ${FZF_TMUX:-1} -eq 1 ] && echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

__fselgit() {
  command git status --short | cut -c4- | $(__fzfcmd) -m
  echo
}

fzf-git-status-widget() {
  LBUFFER="${LBUFFER}$(__fselgit)"
  zle redisplay
}
zle     -N   fzf-git-status-widget
bindkey '^F' fzf-git-status-widget

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  zle redisplay
}
zle     -N   fzf-file-widget
bindkey '^T' fzf-file-widget
