if [[ $OSTYPE == darwin* ]]; then
  export OSX=true
fi

# Set the path to include:
#  - $HOME/.bin      for local tools
#  - /usr/local/bin  for Homebrew
#  - /usr/local/sbin
#  - /usr/bin        for system executables
#  - /usr/sbin
#  - /bin
#  - /sbin
export PATH="$HOME/.bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"

# Set my default editor to Vim :)
vim=$(which vim)
export EDITOR=$vim

if which clang > /dev/null; then
  export CC=clang
fi

# i - Vim's smart case
# j.5 - Center search results
# K - Quit on CTRL-C
# M - Longer prompt
# R - output colors correctly
# X - Don't send clear screen signal
export LESS="ij.5KMRX"

export GREP_OPTIONS="--color"

# Disable ^-S in the terminal
stty -ixon -ixoff

# The directory for maildir mail
export MAILDIR="$HOME/.mail"

# Source something from all shells for private ENV vars
if [[ -f "$HOME/.secrets" ]]; then
  source "$HOME/.secrets"
fi
