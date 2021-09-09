if (( $+commands[tag] )); then
  tag() {
    trap 'source /tmp/tag_aliases 2>/dev/null' SIGINT
    command tag "$@" && source /tmp/tag_aliases 2>/dev/null
    trap - SIGINT
  }
  alias s="tag rg"
  if command -v fdfind >/dev/null 2>&1; then
    alias fd="tag fdfind --hidden"
  else
    alias fd="tag fd --hidden"
  fi
  alias find="tag find"
else
  alias s=rg
fi
