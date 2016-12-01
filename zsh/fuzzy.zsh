find_files() {
  command find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | sed 1d | cut -b3- | fzf | while read item; do
    printf '%q ' "$item"
  done
  echo
}

trim_quotes() {
  echo "$1" | sed -e 's/"$//' -e 's/^"//'
}

fuzzy_git_files_widget() {
  trap "" INT

  result="$(git ls-files "$(git rev-parse --show-toplevel 2>/dev/null)" \
    --cached --exclude-standard --others 2>/dev/null | fzf)"
  if [ "$result" != "" ]; then
    LBUFFER="${LBUFFER}\"$(trim_quotes $result)\""
  fi

  zle redisplay
  trap INT
}

fuzzy_git_status_widget() {
  trap "" INT

  result="$(git status --short | cut -c4- | fzf)"
  if [ -n "$result" ]; then
    LBUFFER="${LBUFFER}\"$(trim_quotes $result)\""
  fi

  zle redisplay
  trap INT
}

fuzzy_file_widget() {
  LBUFFER="${LBUFFER}$(find_files)"
  zle redisplay
}

zle     -N   fuzzy_git_status_widget
bindkey '^F' fuzzy_git_status_widget

zle     -N   fuzzy_file_widget
bindkey '^T' fuzzy_file_widget

zle     -N   fuzzy_git_files_widget
bindkey '^G' fuzzy_git_files_widget
