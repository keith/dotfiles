if (( $+commands[tag] )); then
  tag() {
    trap 'source /tmp/tag_aliases 2>/dev/null' SIGINT
    command tag "$@" && source /tmp/tag_aliases 2>/dev/null
    trap - SIGINT
  }

  alias s="tag rg"
  alias st="tag rg --no-ignore-dot"
  alias find="tag find"

  function fd() {
    binary=fd
    if command -v fdfind >/dev/null 2>&1; then
      binary=fdfind
    fi

    if [[ -f .rgignore ]]; then
      tag "$binary" --hidden --ignore-file .rgignore "$@"
    else
      tag "$binary" --hidden "$@"
    fi
  }

  if command -v fdfind >/dev/null 2>&1; then
    alias fdt="tag fdfind --hidden"
  else
    alias fdt="tag fd --hidden"
  fi
else
  alias s=rg

  if command -v fdfind >/dev/null 2>&1; then
    alias fd="fdfind --hidden"
  else
    alias fd="fd --hidden"
  fi
fi
