# CTRL-R - Paste the selected command from history into the command line
history-widget() {
  LBUFFER=$(fc -l 1 | sed "s/ *[0-9*]* *//" | selecta)
  zle redisplay
}
zle     -N   history-widget
bindkey '^S' history-widget
