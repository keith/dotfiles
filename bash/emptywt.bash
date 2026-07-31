emptywt() {
  local worktree
  worktree="$("$DOTFILES/bin/emptywt" "$@")" || return
  [[ -n "$worktree" ]] || return
  cd "$worktree" || return
}
