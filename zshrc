# Source the major default settings shared
# between zsh and bash

if [[ ! -f $HOME/.shellrc ]];then
  echo "$HOME/.shellrc doesn't exist. Run ./manage.sh install again"
else
  source $HOME/.shellrc
fi

# Find all zsh files
typeset -U configs
configs=($DOTFILES/**/*.zsh $DOTFILES/**/*.bash)

for file in ${configs:#*/completions.zsh}
do
  source $file
done

# Load autocomplete and other zsh stuff
autoload -Uz compinit
compinit -i

for file in ${(M)configs:#*/completions.zsh}
do
  source $file
done

unset configs
