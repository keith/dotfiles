# Only allow loading once
#   to reload run 'unset ZSH_LOADED'
if [[ "$ZSH_LOADED" == true ]];then
  return
fi
export ZSH_LOADED=true

# Path to dotfiles repo
export DOTFILES="$(dirname $(readlink $HOME/.zshrc))"

# Loads a local settings file for sekrets
if [[ -e $HOME/.localrc ]]
then
  source $HOME/.localrc
fi

if [[ $OSTYPE == darwin* ]];then
  export OSX=true
fi

# Set the path to include:
#  - /usr/local/bin for Homebrew and others
#  - $HOME/.bin for local tools
export PATH="/usr/local/bin:$HOME/.bin:$PATH"

# Find all zsh files
typeset -U configs
configs=($DOTFILES/**/*.zsh)

for file in ${configs:#*/completions.zsh}
do
  source $file
done

# Load autocomplete and other zsh stuff
autoload -Uz compinit
compinit -i

for file in ${(M)configs:#*/completions.zsh}
do
  source $file
done

unset configs

