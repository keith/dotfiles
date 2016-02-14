if ! which gpg-agent &> /dev/null; then
  return
fi

# Fix gpg not asking for password because it didn't know how
export GPG_TTY=$(tty)

envfile="$HOME/.gpg-agent-info"
if [ -e "$envfile" ] && kill -0 "$(grep GPG_AGENT_INFO "$envfile" | cut -d: -f 2)" 2>/dev/null; then
  eval "$(cat "$envfile")"
else
  eval "$(gpg-agent --daemon --enable-ssh-support --write-env-file "$envfile")"
fi
export GPG_AGENT_INFO  # the env file does not contain the export statement
export SSH_AUTH_SOCK   # enable gpg-agent for ssh
