# Source the major default settings shared
# between zsh and bash
source $HOME/.shellrc

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
if [[ -e $HOME/.coloroverride ]];then
  colorize $(cat $HOME/.coloroverride)
else
  colorize
fi

