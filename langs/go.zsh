# Setup golang.org variables
if [[ "$OSX" == true ]];then
  export GOROOT=$(brew --prefix go)
else
  echo "Need to define \$GOROOT"
  export GOROOT
fi

# Setup path to Go projects
export GOPATH=$HOME/Go

# Add Go's bin to PATH
PATH="$GOPATH/bin:$PATH"

