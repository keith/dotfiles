# The path of rbenv
export RBENVPATH="$HOME/.rbenv"

if [[ -d $RBENVPATH ]];then
  # Add rbenv to PATH
  PATH="$RBENVPATH/bin:$PATH"  

  # Load rbenv on launch
  eval "$(rbenv init -)"
else
  echo "rbenv is not installed. Run '$DOTFILES/rbenv.sh install'"
fi

