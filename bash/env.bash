if [[ $OSTYPE == darwin* ]]; then
  export OSX=true
fi

# Set the path to include:
#  - $HOME/.bin       for local tools
#  - $HOME/.local/bin for haskell tools installed by stack
#  - /usr/local/bin   for Homebrew
#  - /usr/local/sbin
#  - /usr/bin         for system executables
#  - /usr/sbin
#  - /bin
#  - /sbin
export PATH="$HOME/.bin:$HOME/.local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"

export EDITOR=nvim

# i - Vim's smart case
# j.5 - Center search results
# K - Quit on CTRL-C
# M - Longer prompt
# R - output colors correctly
# X - Don't send clear screen signal
export LESS="ij.5KMRX"
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

export PIP_DISABLE_PIP_VERSION_CHECK=1
export SCCACHE_CACHE_SIZE=100G
