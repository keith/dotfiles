find_files() {
  command find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
    -o -type f -print \
    -o -type l -print 2> /dev/null | sed 1d | cut -b3- | fzy | while read item; do
    printf '%q ' "$item"
  done
  echo
}

trim_quotes() {
  echo "$1" | sed -e 's/"$//' -e 's/^"//'
}

fuzzy_git_files_widget() {
  trap "" INT

  result="$(find-files-async | fzy)"
  if [ "$result" != "" ]; then
    LBUFFER="${LBUFFER}\"$(trim_quotes $result)\""
  fi

  zle redisplay
  trap INT
}

fuzzy_git_changed_files_widget() {
  trap "" INT

  result="$(git dm --name-only | fzy)"
  if [ "$result" != "" ]; then
    LBUFFER="${LBUFFER}\"$(trim_quotes $result)\""
  fi

  zle redisplay
  trap INT
}

fuzzy_git_status_widget() {
  trap "" INT

  # TODO: do renames / copies work?
  result="$(git status -s | cut -c4- | fzy)"

  if [ -n "$result" ]; then
    LBUFFER="${LBUFFER}$result"
  fi

  zle redisplay
  trap INT
}

fuzzy_file_widget() {
  LBUFFER="${LBUFFER}$(find_files)"
  zle redisplay
}

fuzzy_history_widget() {
  LBUFFER="$(fc -l 1 | cut -c 8- | tac | fzy)"
  zle redisplay
}

fuzzy_log_widget() {
  trap "" INT

  result="$(git log -50 --format="%h %s" | fzy)"
  if [[ -n "$result" ]]; then
    LBUFFER="${LBUFFER}$(echo "$result" | cut -d " " -f 1)"
  fi

  zle redisplay
  trap INT
}

zle     -N   fuzzy_git_status_widget
bindkey '^F' fuzzy_git_status_widget

zle     -N   fuzzy_file_widget
bindkey '^T' fuzzy_file_widget

zle     -N   fuzzy_git_files_widget
bindkey '^G' fuzzy_git_files_widget

zle     -N   fuzzy_git_changed_files_widget
bindkey '^O' fuzzy_git_changed_files_widget

zle     -N   fuzzy_history_widget
bindkey '^U' fuzzy_history_widget

zle     -N   fuzzy_log_widget
bindkey '^N' fuzzy_log_widget
