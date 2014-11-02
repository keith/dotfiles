# Path to dotfiles repo
export DOTFILES="$(dirname $(readlink $HOME/.zshrc))"

# Find all zsh files
typeset -U configs
configs=($DOTFILES/*/*.bash $DOTFILES/*/*.zsh)
for file in ${configs:#*/completions.zsh}
do
  source $file
done

# Load autocomplete and other zsh stuff
autoload -Uz compinit
compinit -i
source $DOTFILES/zsh/completions.zsh

unset configs
