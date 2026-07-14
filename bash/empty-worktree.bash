empty-worktree() {
  local worktree
  worktree="$("$DOTFILES/bin/empty-worktree" "$@")" || return
  [[ -n "$worktree" ]] || return
  cd "$worktree" || return
}
