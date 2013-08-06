# The path of rbenv
export RBENVPATH="$HOME/.rbenv"

if [[ ! -d $RBENVPATH ]];then
  echo "rbenv is not installed. Run '$DOTFILES/rbenv.sh install'"
  return
fi

# Add rbenv to PATH
PATH="$RBENVPATH/bin:$PATH"  

# Load rbenv on launch
eval "$(rbenv init -)"

# Ruby/Rails
alias ruby="ruby -w"
alias bi="bundle install"
alias bdgem="gem build *.gemspec; gem install *.gem --no-ri --no-rdoc; rbenv rehash"
alias rr="rbenv rehash"
alias rake="noglob rake"
alias binstubs="gem regenerate_binstubs"
alias coveron="export COVERALLS_RUN_LOCALLY=true"
alias coveroff="unset COVERALLS_RUN_LOCALLY"

