# Make sure we're running OS X
if [[ "$OSX" != true ]];then
  return
fi

# Link to zsh-completions files from homebrew
fpath=($(brew --prefix)/share/zsh-completions $(brew --prefix)/share/zsh/site-functions $fpath)

