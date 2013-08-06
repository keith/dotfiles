# Setup golang.org variables
if [[ "$OSX" == true ]];then
  export GOROOT=$(brew --prefix go)
else
  echo "Need to define \$GOROOT"
  export GOROOT
fi

export GOPATH=$HOME/Go
PATH="$GOPATH/bin:$PATH"

