# Setup path to Go projects
export GOPATH=$HOME/gophers
# Add Go projects bin to PATH
PATH="$GOPATH/bin:$PATH"

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
