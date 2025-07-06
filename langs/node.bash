export NVM_DIR="$HOME/.nvm"
if [[ -d "$NVM_DIR" ]]; then
  source "$NVM_DIR/nvm.sh"
elif [[ -s "$BREW_PREFIX/opt/nvm/nvm.sh" ]]; then
  source "$BREW_PREFIX/opt/nvm/nvm.sh"
fi
