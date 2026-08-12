if (( $+commands[atuin] )); then
  atuin_cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}/zsh
  atuin_cache_file=$atuin_cache_dir/atuin-init.zsh
  atuin_config_file=$HOME/.config/atuin/config.toml
  if [[ ! -s $atuin_cache_file || $commands[atuin] -nt $atuin_cache_file || $atuin_config_file -nt $atuin_cache_file ]]; then
    command mkdir -p "$atuin_cache_dir"
    atuin_cache_tmp=$atuin_cache_file.$$
    if atuin init zsh --disable-up-arrow --disable-ctrl-r >! "$atuin_cache_tmp"; then
      command mv "$atuin_cache_tmp" "$atuin_cache_file"
    else
      command rm -f "$atuin_cache_tmp"
    fi
  fi

  if [[ -s $atuin_cache_file ]]; then
    source "$atuin_cache_file"
  else
    eval "$(atuin init zsh --disable-up-arrow --disable-ctrl-r)"
  fi
  unset atuin_cache_dir atuin_cache_file atuin_cache_tmp atuin_config_file
fi
