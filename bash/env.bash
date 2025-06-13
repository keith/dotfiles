if [[ $OSTYPE == darwin* ]]; then
  export OSX=true
fi

# Set the path to include:
#  - $HOME/.bin       for local tools
#  - $HOME/.local/bin for haskell tools installed by stack
#  - $HOME/.cargo/bin for rust tools installed by cargo
#  - $BREW_PREFIX/bin for Homebrew on Apple Silicon, or duplicates the /usr/local/bin path
#  - /usr/local/bin
#  - /usr/local/sbin
#  - /usr/bin         for system executables
#  - /usr/sbin
#  - /bin
#  - /sbin
export PATH="$HOME/dev/keith/knox:$DOTFILES/git-pile/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.cargo/bin:$BREW_PREFIX/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
dev_subdirs=$(printf "%s:" ~/dev/*)
export CDPATH="${dev_subdirs}$HOME/dev"

export EDITOR=nvim

# i - Vim's smart case
# j.5 - Center search results
# K - Quit on CTRL-C
# M - Longer prompt
# R - output colors correctly
# X - Don't send clear screen signal
export LESS="ij.5KMRX"
export MANPAGER="$EDITOR +Man\!"
# On macOS _something_ is setting this which breaks default behavior like 'man
# libtool' pointing at Xcode
export MANPATH=

export PAGER="less"
export RIPGREP_CONFIG_PATH="$HOME/.config/rgrc.conf"

# Disable ^-S in the terminal
stty -ixon -ixoff

# The directory for maildir mail
export MAILDIR="$HOME/.mail"

# Force gpg-agent to use the current tty
tty_path=$(tty)
export GPG_TTY=$tty_path

# Source something from all shells for private ENV vars
if [[ -f "$HOME/.secrets" ]]; then
  source "$HOME/.secrets"
fi

export CARGO_REGISTRIES_CRATES_IO_PROTOCOL=sparse
export PIP_DISABLE_PIP_VERSION_CHECK=1

# Swift compiler sccache setup
export SCCACHE_CACHE_SIZE=100G
export SWIFT_USE_SCCACHE=1

export GIT_PILE_PREFIX=ks/
export GIT_PILE_VERBOSE=true

export KUBECTL_EXTERNAL_DIFF=d

export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_EMOJI=1
export HOMEBREW_NO_INSTALL_CLEANUP=1

export GH_NO_UPDATE_NOTIFIER=1
export GH_BROWSER=open

export FZF_DEFAULT_OPTS="--layout=reverse --height=11 --bind ctrl-j:accept"

ulimit -f unlimited
