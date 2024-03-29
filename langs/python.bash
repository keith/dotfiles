export PYTHONSTARTUP=$DOTFILES/langs/pystartup.py
export VIRTUAL_ENV_DISABLE_PROMPT=1

# https://github.com/pyenv/pyenv/issues/1649#issuecomment-694388530
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH"
export PYENV_SHELL=zsh
