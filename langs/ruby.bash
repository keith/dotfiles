export GEM_HOME=$HOME/.gem
export GEM_PATH=$GEM_HOME
export PATH=$GEM_HOME/bin:$PATH

readonly chruby_root="$BREW_PREFIX/opt/chruby"
if [[ -d $chruby_root && -d ~/.rubies ]]; then
  source "$chruby_root/share/chruby/chruby.sh"
  if [[ -n ${ZSH_VERSION:-} ]]; then
    eval 'ruby_entries=("$HOME"/.rubies/*(N))'
    ruby_name=${ruby_entries[1]:t}
  else
    ruby_name="$(ls ~/.rubies | sort | head -1)"
  fi
  chruby_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh"
  chruby_cache="$chruby_cache_dir/chruby-$ruby_name"

  # Starting Ruby just to discover these stable values costs about 30ms.
  # Reuse them until either chruby or the selected Ruby installation changes.
  if [[ -r $chruby_cache && $chruby_cache -nt $chruby_root/share/chruby/chruby.sh ]]; then
    IFS='|' read -r cached_ruby_root cached_ruby_engine cached_ruby_version cached_gem_root < "$chruby_cache"
  fi

  if [[ -n $cached_ruby_root && -n $cached_ruby_engine && -n $cached_ruby_version && -n $cached_gem_root && -x $cached_ruby_root/bin/ruby && $chruby_cache -nt $cached_ruby_root && $chruby_cache -nt $cached_ruby_root/bin/ruby ]]; then
    [[ -n $RUBY_ROOT ]] && chruby_reset
    export RUBY_ROOT="$cached_ruby_root"
    export RUBYOPT=
    export RUBY_ENGINE="$cached_ruby_engine"
    export RUBY_VERSION="$cached_ruby_version"
    export GEM_ROOT="$cached_gem_root"
    export GEM_HOME="$HOME/.gem/$RUBY_ENGINE/$RUBY_VERSION"
    export GEM_PATH="$GEM_HOME${GEM_ROOT:+:$GEM_ROOT}${GEM_PATH:+:$GEM_PATH}"
    export PATH="$GEM_HOME/bin${GEM_ROOT:+:$GEM_ROOT/bin}:$RUBY_ROOT/bin:$PATH"
    hash -r
  else
    chruby "$ruby_name"
    mkdir -p "$chruby_cache_dir"
    printf '%s|%s|%s|%s\n' "$RUBY_ROOT" "$RUBY_ENGINE" "$RUBY_VERSION" "$GEM_ROOT" > "$chruby_cache"
  fi

  unset cached_gem_root cached_ruby_engine cached_ruby_root cached_ruby_version chruby_cache chruby_cache_dir ruby_entries ruby_name
fi

alias bi="bundle install"
alias be="bundle exec"
