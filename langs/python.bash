export PYTHONPATH=.:$(which python):$PYTHONPATH
export PYTHONSTARTUP=$DOTFILES/langs/pystartup

export PYENV_ROOT="$HOME/.pyenv"
if [[ -d "$PYENV_ROOT" ]]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi
