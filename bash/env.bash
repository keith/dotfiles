if [[ $OSTYPE == darwin* ]];then
  export OSX=true
fi

# Set the path to include:
#  - /usr/local/bin  for Homebrew
#  - /usr/local/sbin
#  - /usr/bin        for system executables
#  - /bin
#  - /usr/sbin
#  - /sbin
#  - $HOME/.bin      for local tools
#  - .git/safe/../../bin  mkdir .git/safe in the root of repositories you trust
export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.bin:.git/safe/../../bin"


# Set my default editor to Vim :)
vim=$(which vim)
export EDITOR=$vim

if command -v clang &> /dev/null;then
  export CC=clang
fi

# i - Vim's smart case
# j.5 - Center search results
# K - Quit on CTRL-C
# M - Longer prompt
# R - output colors correctly
# X - Don't send clear screen signal
export LESS="ij.5KMRX"

# Disable ^-S in the terminal
stty -ixon -ixoff
