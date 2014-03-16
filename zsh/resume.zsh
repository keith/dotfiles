# https://github.com/joshuaclayton/dotfiles/blob/master/zsh_profile.d/navigation.zsh
export CURRENT_PROJECT_PATH=$HOME/.cwd

__setdir() {
  echo $(pwd) >! $CURRENT_PROJECT_PATH
}
if [[ -z "$SSH_CLIENT" ]];then
  chpwd_functions=($chpwd_functions __setdir)
fi

cd_to_most_recently_opened_directory() {
  if [[ -f $CURRENT_PROJECT_PATH && -d "$(cat $CURRENT_PROJECT_PATH)" ]]; then
    cd "$(cat $CURRENT_PROJECT_PATH)"
  fi
}

if [[ -z "$TMUX" && -z "$SSH_CLIENT" ]];then
  cd_to_most_recently_opened_directory
fi
