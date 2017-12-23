export RG_DEFAULT_OPTIONS="--colors 'match:bg:yellow' --colors 'match:fg:black' --colors 'path:fg:green'"

if (( $+commands[tag] )); then
  tag() { command tag "$@" && source /tmp/tag_aliases_$USER 2>/dev/null }
  alias ag="tag ag"
  alias rg="tag rg $RG_DEFAULT_OPTIONS"
else
  alias rg="rg $RG_DEFAULT_OPTIONS"
fi
