if (( $+commands[tag] )); then
  tag() {
    trap 'source /tmp/tag_aliases 2>/dev/null' SIGINT
    command tag "$@" && source /tmp/tag_aliases 2>/dev/null
    trap - SIGINT
  }
  alias s="tag rg"
else
  alias s=rg
fi

alias ag="echo 'use s'"
alias rg="echo 'use s'"
