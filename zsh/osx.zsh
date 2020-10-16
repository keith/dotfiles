if [ -z $OSX ]; then
  return
fi

# Link zsh completion files from homebrew
fpath=(/usr/local/share/zsh/site-functions $fpath)
fpath=(/usr/local/share/zsh-completions $fpath)
fpath=($HOME/.nix-profile/share/zsh/site-functions $fpath)
