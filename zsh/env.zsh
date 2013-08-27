# Set my default editor to Vim :)
export EDITOR=$(which vim)

if which clang &> /dev/null;then
  export CC=clang
fi

