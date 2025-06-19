# Path to dotfiles repo
export DOTFILES="$(dirname "$(readlink "$HOME/.zshrc")")"
if [[ $OSTYPE != darwin* ]]; then
  export BREW_PREFIX=/home/linuxbrew/.linuxbrew
else
  export BREW_PREFIX=/opt/homebrew
fi

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
compinit -C

for file in ${(M)configs:#*/completions.zsh}
do
  source "$file"
done

if [[ "$(wc -l ~/.keith_zsh_history | xargs | cut -d " " -f 1)" -lt 1000 ]]; then
  echo "warning: ~/.keith_zsh_history looks borked"
fi

old_line_count=0
if [[ -f ~/.hist_line_count ]]; then
  old_line_count=$(cat ~/.hist_line_count)
fi

line_count=$(wc -l ~/.keith_zsh_history | xargs | cut -d " " -f 1)
# -some to give some buffer as history isn't always written immediately from old terminals it seems
if [[ $line_count -lt $((old_line_count - 1000)) ]]; then
  echo "warning: history count just went down, was it truncated? Went from $old_line_count to $line_count"
fi

echo $line_count > ~/.hist_line_count

if [[ -e "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh" ]]; then
  source "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
fi

if command -v atuin > /dev/null; then
  eval "$(atuin init zsh --disable-up-arrow --disable-ctrl-r)"
fi

set-alacritty-theme
