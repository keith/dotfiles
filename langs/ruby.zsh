if [[ ! -d $chruby_root || ! -d $HOME/.rubies ]]; then
  return
fi

chruby_script=$chruby_root/share/chruby/chruby.sh
IFS= read -r chruby_version_line < "$chruby_script"
eval "$chruby_version_line"
unset chruby_version_line

RUBIES=("$HOME/.rubies"/*(N))

_chruby_load() {
  unset -f chruby chruby_reset chruby_use
  source "$chruby_script"
  local load_result=$?
  unset chruby_script
  unset -f _chruby_load
  return $load_result
}

chruby() {
  _chruby_load && chruby "$@"
}

chruby_reset() {
  _chruby_load && chruby_reset "$@"
}

chruby_use() {
  _chruby_load && chruby_use "$@"
}

ruby_root=$RUBIES[1]
ruby_name=${ruby_root:t}

if [[ -n $ruby_root && -x $ruby_root/bin/ruby ]]; then
  ruby_cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}/zsh
  ruby_cache_file=$ruby_cache_dir/chruby-${ruby_root:t}.zsh

  if [[ ! -s $ruby_cache_file || $ruby_root/bin/ruby -nt $ruby_cache_file ]]; then
    command mkdir -p "$ruby_cache_dir"
    ruby_cache_tmp=$ruby_cache_file.$$
    if "$ruby_root/bin/ruby" - >! "$ruby_cache_tmp" <<'RUBY'
puts "export RUBY_ENGINE=#{defined?(RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'};"
puts "export RUBY_VERSION=#{RUBY_VERSION};"
begin
  require 'rubygems'
  puts "export GEM_ROOT=#{Gem.default_dir.inspect};"
rescue LoadError
end
RUBY
    then
      command mv "$ruby_cache_tmp" "$ruby_cache_file"
    else
      command rm -f "$ruby_cache_tmp"
    fi
  fi

  if [[ -s $ruby_cache_file ]]; then
    if [[ -n $_chruby_reset_environment ]]; then
      PATH=":$PATH:"
      PATH="${PATH//:$RUBY_ROOT\/bin:/:}"
      if (( UID != 0 )); then
        [[ -n $GEM_HOME ]] && PATH="${PATH//:$GEM_HOME\/bin:/:}"
        [[ -n $GEM_ROOT ]] && PATH="${PATH//:$GEM_ROOT\/bin:/:}"

        GEM_PATH=":$GEM_PATH:"
        [[ -n $GEM_HOME ]] && GEM_PATH="${GEM_PATH//:$GEM_HOME:/:}"
        [[ -n $GEM_ROOT ]] && GEM_PATH="${GEM_PATH//:$GEM_ROOT:/:}"
        GEM_PATH=${GEM_PATH#:}
        GEM_PATH=${GEM_PATH%:}
        unset GEM_ROOT GEM_HOME
        [[ -z $GEM_PATH ]] && unset GEM_PATH
      fi

      PATH=${PATH#:}
      PATH=${PATH%:}
      unset RUBY_ROOT RUBY_ENGINE RUBY_VERSION RUBYOPT
      hash -r
    fi

    export RUBY_ROOT=$ruby_root
    export RUBYOPT=
    export PATH="$RUBY_ROOT/bin:$PATH"
    source "$ruby_cache_file"

    if (( UID != 0 )); then
      export GEM_HOME="$HOME/.gem/$RUBY_ENGINE/$RUBY_VERSION"
      export GEM_PATH="$GEM_HOME${GEM_ROOT:+:$GEM_ROOT}${GEM_PATH:+:$GEM_PATH}"
      export PATH="$GEM_HOME/bin${GEM_ROOT:+:$GEM_ROOT/bin}:$PATH"
    fi
  else
    chruby "$ruby_name"
  fi
fi

unset _chruby_reset_environment ruby_cache_dir ruby_cache_file ruby_cache_tmp ruby_name ruby_root
