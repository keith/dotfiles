export GEM_HOME=$HOME/.gem
export GEM_PATH=$GEM_HOME
export PATH=$GEM_HOME/bin:$PATH

CHRUBY="/usr/local/opt/chruby"
if [[ -d $CHRUBY ]]; then
  source "$CHRUBY/share/chruby/chruby.sh"
  chruby "$(chruby | sort | tail -1 | tr -d " *")"
fi

alias bi="bundle install"
alias be="bundle exec"
