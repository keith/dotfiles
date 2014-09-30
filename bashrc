# Path to dotfiles repo
export DOTFILES="$(dirname $(readlink $HOME/.bashrc))"

# i - Vim's smart case
# j.5 - Center search results
# K - Quit on CTRL-C
# M - Longer prompt
# R - output colors correctly
# X - Don't send clear screen signal
export LESS="ij.5KMRX"

# Loads a local settings file for sekrets
if [[ -e $HOME/.localrc ]];then
  source $HOME/.localrc
fi

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

# Disable ^-S in the terminal
stty -ixon -ixoff

configs=($DOTFILES/*/*.bash)
for file in ${configs[@]}
do
  source $file
done

export PS1="(\w \j) \$ "
