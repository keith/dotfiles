if (( $+commands[tag] )); then
  tag() {
    trap 'source /tmp/tag_aliases 2>/dev/null' SIGINT
    command tag "$@" && source /tmp/tag_aliases 2>/dev/null
    trap - SIGINT
  }
  alias s="tag rg"
  alias fd="tag fd"
  alias find="tag find"
else
  alias s=rg
fi
