source $DOTFILES/scripts/nvm/nvm.sh

# Check platform for node location
if [[ "$OSX" == true ]];then
  # Add node modules to PATH
  PATH="$(brew --prefix)/share/npm/bin:$PATH"
fi

