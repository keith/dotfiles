# Setup golang.org variables
if [[ "$OSX" == true ]];then
  export GOROOT=$(brew --prefix go)
else
  echo "Need to define \$GOROOT"
  export GOROOT="/usr/local/src/go"
fi

# Setup path to Go projects
export GOPATH=$HOME/Go

# Add Go's bin to PATH
PATH="$GOPATH/bin:$PATH"

if ! which go &> /dev/null;then
  PATH="$GOROOT/bin:$PATH"
fi

