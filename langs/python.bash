export PYTHONPATH=.:$(which python):$PYTHONPATH
export PYTHONSTARTUP=$DOTFILES/langs/pystartup

PYENV_ROOT="$HOME/.pyenv"
if [[ -d "$PYENV_ROOT" ]]; then
  export PYENV_ROOT
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
fi
