opp=$(brew --prefix opp 2>/dev/null)
file="$opp/opp.zsh"

if [ ! -f "$file" ]; then
  return
fi

source "$file"
bindkey -M vicmd '~' vi-swap-case
