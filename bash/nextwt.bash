nextwt() {
  local worktree
  worktree="$("$DOTFILES/bin/nextwt" "$@")" || return
  [[ -n "$worktree" ]] || return
  cd "$worktree" || return
}
