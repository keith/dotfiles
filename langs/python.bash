export PYTHONSTARTUP=$DOTFILES/langs/pystartup.py
export VIRTUAL_ENV_DISABLE_PROMPT=1

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv >/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
