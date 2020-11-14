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
