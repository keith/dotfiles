# Setup path to Go projects
export GOPATH=$HOME/gophers
# Add Go projects bin to PATH
PATH="$GOPATH/bin:$PATH"

# Go version manager configs, for the future
# goenv=$HOME/.goenv
# if [[ -d $goenv ]]; then
#     export PATH="$goenv/bin:$PATH"
#     eval "$(goenv init -)"
#     unset goenv
#     return
# fi

# export GVM_ROOT=$HOME/.gvm
# if [[ -d $GVM_ROOT ]]; then
#     source $GVM_ROOT/scripts/gvm-default
#     return
# else
#     unset GVM_ROOT
# fi

if ! which go &> /dev/null; then
  if [[ "$OSX" == true ]]; then
    go_root=$(brew --prefix go)
    export GOROOT=$go_root/libexec
  else
    export GOROOT="/usr/local/go"
  fi

  if [[ -d $GOROOT ]]; then
    PATH="$GOROOT/bin:$PATH"
  else
    unset GOROOT
  fi
fi
