# Source the major default settings shared
# between zsh and bash
source $HOME/.shellrc

configs=($DOTFILES/**/*.bash)
for file in ${configs}
do
  source $file
done

