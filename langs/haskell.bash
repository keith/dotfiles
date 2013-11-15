# Check that cabal has been setup
if [[ -d "$HOME/.cabal" ]];then
  # Add cabal installed packages to PATH
  PATH="$HOME/.cabal/bin:$PATH"
fi

