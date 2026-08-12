if [[ -n ${ZSH_VERSION:-} && -n ${RUBY_ROOT:-} ]]; then
  _chruby_reset_environment=1
fi

export GEM_HOME=$HOME/.gem
export GEM_PATH=$GEM_HOME
export PATH=$GEM_HOME/bin:$PATH

readonly chruby_root="$BREW_PREFIX/opt/chruby"
if [[ -z ${ZSH_VERSION:-} && -d $chruby_root && -d ~/.rubies ]]; then
  source "$chruby_root/share/chruby/chruby.sh"
  chruby "$(ls ~/.rubies | sort | head -1)"
fi

alias bi="bundle install"
alias be="bundle exec"
