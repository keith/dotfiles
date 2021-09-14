# matches case insensitive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending

# Ignore compiled files on vi/vim completion
zstyle ':completion:*:*:(v|vim|nvim):*:*files' ignored-patterns '*.(a|dylib|so|o|pyc|resolved)'

# Ignore pyc files for python
zstyle ':completion:*:*:(python*|pytest):*:*files' ignored-patterns '*.(pyc)'

# Don't complete stuff already being used
zstyle ':completion::*:(v|vim|rm|srm):*' ignore-line true

# Cache to increase speed
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.zsh/tmp/cache"

# Explicitly write the type of what autocomplete has found / was looking for
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:warnings' format 'No matches for: %d'

# Setup kubectl alias with completion
alias compdef k="kubectl"

# https://unix.stackexchange.com/a/310382/30131
_g () {
  case "${words[2]}" in
    co) words[1,2]=(git checkout);;
  esac

  _git # Delegate to completion
}
compdef _g g
compdef _g git
