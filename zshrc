# Path to dotfiles repo
export DOTFILES="$(dirname "$(readlink "$HOME/.zshrc")")"

# temporary workaround for https://stackoverflow.com/questions/15454174/how-can-a-shell-function-know-if-it-is-running-within-a-virtualenv#comment31587975_15454284
unset VIRTUAL_ENV

# Find all zsh files
configs=($DOTFILES/*/*.bash $DOTFILES/*/*.zsh)
for file in ${configs:#*/completions.zsh}
do
  source "$file"
done

# Load autocomplete and other zsh stuff
autoload -Uz compinit
compinit -i

for file in ${(M)configs:#*/completions.zsh}
do
  source "$file"
done

if [[ "$(wc -l ~/.keith_zsh_history | xargs | cut -d " " -f 1)" -lt 1000 ]]; then
  echo "warning: ~/.keith_zsh_history looks borked"
fi

set-alacritty-theme
