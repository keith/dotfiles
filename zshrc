# Source the major default settings shared
# between zsh and bash
if [[ ! -f $HOME/.bashrc ]];then
  echo "$HOME/.bashrc doesn't exist. Run ./manage.sh install again"
else
  source $HOME/.bashrc
fi

# Find all zsh files
typeset -U configs
configs=($DOTFILES/*/*.zsh)
for file in ${configs:#*/completions.zsh}
do
  source $file
done

# Load autocomplete and other zsh stuff
autoload -Uz compinit
compinit -i
source $DOTFILES/zsh/completions.zsh

unset configs
