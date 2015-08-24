CHRUBY="/usr/local/opt/chruby"
RBENVPATH="$HOME/.rbenv"
RVMPATH="$HOME/.rvm"

if [[ -d $CHRUBY ]]; then
  source $CHRUBY/share/chruby/chruby.sh
  chruby "$(chruby | sort | tail -1 | tr -d " *")"
elif [[ -d $RBENVPATH ]]; then
  PATH="$RBENVPATH/bin:$PATH"
  eval "$(rbenv init - --no-rehash)"
elif [[ -d $RVMPATH ]]; then
  PATH="$RVMPATH/bin:$PATH"
else
  return
fi

alias bi="bundle install"
alias bdgem="gem build *.gemspec; gem install *.gem --no-ri --no-rdoc"
