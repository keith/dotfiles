emptywt() {
  if [[ $# -gt 0 && "$1" != -* ]]; then
    cd -- "$1" || return
    shift
  fi

  local worktree
  worktree="$("$DOTFILES/bin/emptywt" "$@")" || return
  [[ -n "$worktree" ]] || return
  cd "$worktree" || return
}
