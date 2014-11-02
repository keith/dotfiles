typeset -U opp_configs
opp_configs=($DOTFILES/scripts/opp/*.zsh $DOTFILES/scripts/opp/*/*.zsh)

for file in ${opp_configs:#*/test*.zsh}
do
  source $file
done

bindkey -M vicmd '~' vi-swap-case

unset opp_configs
