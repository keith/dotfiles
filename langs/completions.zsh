# Completions for language tools
paths=($GOROOT/share/zsh/site-functions/go)
for path in ${paths}
do
  if [[ -d $path ]];then
    source $path
  fi
done

