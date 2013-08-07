# Completions for language tools
completions=($GOROOT/share/zsh/site-functions/go)
for completion in ${completions}
do
  if [[ -d $completion ]];then
    source $completion
  fi
done

