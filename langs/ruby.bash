export GEM_HOME=$HOME/.gem
export GEM_PATH=$GEM_HOME
export PATH=$GEM_HOME/bin:$PATH

readonly chruby_root="$BREW_PREFIX/opt/chruby"
if [[ -d $chruby_root ]]; then
  source "$chruby_root/share/chruby/chruby.sh"
  chruby "$(ls ~/.rubies | sort | head -1)"
fi

alias bi="bundle install"
alias be="bundle exec"
