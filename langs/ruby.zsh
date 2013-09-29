# The path of rbenv
local RBENVPATH="$HOME/.rbenv"
local HAS_RUBY=false

if [[ -d $RBENVPATH ]];then
  # Add rbenv to PATH
  PATH="$RBENVPATH/bin:$PATH"

  # Load rbenv on launch
  eval "$(rbenv init -)"

  HAS_RUBY=true
fi

local RVMPATH="$HOME/.rvm"

if [[ $HAS_RUBY == false && -d $RVMPATH ]];then
  PATH="$RVMPATH/bin:$PATH"
fi

if $HAS_RUBY;then
  # Ruby/Rails
  alias bi="bundle install"
  alias bdgem="gem build *.gemspec; gem install *.gem --no-ri --no-rdoc; rbenv rehash"
  alias rake="noglob rake"
  alias binstubs="gem regenerate_binstubs"
  alias coveron="export COVERALLS_RUN_LOCALLY=true"
  alias coveroff="unset COVERALLS_RUN_LOCALLY"
fi

