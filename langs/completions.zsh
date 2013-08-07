# Completions for language tools
completions=($GOROOT/share/zsh/site-functions/go $GOROOT/misc/zsh/go)
for completion in ${completions}
do
  if [[ -e $completion ]];then
    source $completion
  fi
done

