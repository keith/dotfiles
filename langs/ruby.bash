RBENVPATH="$HOME/.rbenv"
RVMPATH="$HOME/.rvm"

if [[ -d $RBENVPATH ]];then
  PATH="$RBENVPATH/bin:$PATH"
  eval "$(rbenv init - --no-rehash)"
elif [[ -d $RVMPATH ]];then
  PATH="$RVMPATH/bin:$PATH"
else
  return
fi

alias bi="bundle install"
alias bdgem="gem build *.gemspec; gem install *.gem --no-ri --no-rdoc"
