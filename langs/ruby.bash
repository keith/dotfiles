export GEM_HOME=$HOME/.gem
export GEM_PATH=$GEM_HOME
export PATH=$GEM_HOME/bin:$PATH

readonly chruby_root="$BREW_PREFIX/opt/chruby"
if [[ -d $chruby_root ]]; then
  source "$chruby_root/share/chruby/chruby.sh"
  chruby "$(chruby | sort | tail -1 | tr -d " *")"
fi

alias bi="bundle install"
alias be="bundle exec"
