if (( $+commands[tag] )); then
  tag() { command tag "$@" && source /tmp/tag_aliases_root 2>/dev/null }
  alias ag="echo 'use s'"
  alias s="tag rg"
  alias rg="echo 'use s'"
else
  alias ag=rg
fi
