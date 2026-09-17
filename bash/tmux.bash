tmux-claude() {
  CDPATH="${CDPATH:-}" command tmux-agent "claude --dangerously-skip-permissions" "-r" "$@"
}

tmux-codex() {
  CDPATH="${CDPATH:-}" command tmux-agent "codex --yolo" "resume" "$@"
}
