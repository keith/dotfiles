if (( $+commands[tag] )); then
  tag() { command tag "$@" && source /tmp/tag_aliases_root 2>/dev/null }
  alias s="tag rg"
else
  alias s=rg
fi

alias ag="echo 'use s'"
alias rg="echo 'use s'"
