__fsel() {
  command find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | sed 1d | cut -b3- | $(__fuzzycmd) | while read item; do
    printf '%q ' "$item"
  done
  echo
}

__fuzzycmd() {
  # [ ${FZF_TMUX:-1} -eq 1 ] && echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
  echo "pick"
}

fuzzy-git-files-widget() {
  trap "" INT

  result="$(git ls-files "$(git rev-parse --show-toplevel 2>/dev/null)" \
    --cached --exclude-standard --others 2>/dev/null | $(__fuzzycmd))"
  if [[ "$result" != "" ]]; then
    LBUFFER="${LBUFFER}\"$result\""
    zle redisplay
  fi

  trap INT
}

fuzzy-git-status-widget() {
  trap "" INT

  result="$(git status --short | cut -c4- | $(__fuzzycmd))"
  if [ -n "$result" ]; then
    LBUFFER="${LBUFFER}\"$result\""
    zle redisplay
  fi

  trap INT
}

fuzzy-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  zle redisplay
}

zle     -N   fuzzy-git-status-widget
bindkey '^F' fuzzy-git-status-widget

zle     -N   fuzzy-file-widget
bindkey '^T' fuzzy-file-widget

zle     -N   fuzzy-git-files-widget
bindkey '^G' fuzzy-git-files-widget
