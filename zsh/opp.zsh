typeset -U configs
configs=($DOTFILES/scripts/opp/*.zsh $DOTFILES/scripts/opp/*/*.zsh)

for file in ${configs:#*/test*.zsh}
do
  source $file
done

bindkey -M vicmd '~' vi-swap-case
