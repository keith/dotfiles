# Check that cabal has been setup
if [[ -d "$HOME/.cabal" ]]; then
  # Add cabal installed packages to PATH
  PATH="$HOME/.cabal/bin:$PATH"
fi

# If haskell is installed through GUI haskell-platform
if [[ -d "$HOME/Library/Haskell/bin" ]]; then
  PATH="$HOME/Library/Haskell/bin:$PATH"
fi
