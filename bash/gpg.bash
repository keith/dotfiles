# Fix gpg not asking for password because it didn't know how
export GPG_TTY=$(tty)

# gpg-agent configuration
if [ -f "$HOME/.gpg-agent-info" ]; then
  source "$HOME/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
fi
