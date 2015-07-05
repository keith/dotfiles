if [ -z $OSX ]; then
  return
fi

# Link zsh completion files from homebrew
fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
fpath=($(brew --prefix)/share/zsh-completions $fpath)
