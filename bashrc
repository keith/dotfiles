# Path to dotfiles repo
export DOTFILES="$(dirname $(readlink $HOME/$(basename $0)))"

configs=($DOTFILES/*/*.bash)
for file in ${configs[@]}
do
  source $file
done

export PS1="(\w \j) \$ "
