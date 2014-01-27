# Source the major default settings shared
# between zsh and bash
if [[ ! -f $HOME/.shellrc ]];then
  echo "$HOME/.shellrc doesn't exist. Run ./manage.sh install again"
else
  source $HOME/.shellrc
fi

configs=($DOTFILES/**/*.bash)
for file in ${configs[@]}
do
  source $file
done

export PS1="(\w \j) \$ "
