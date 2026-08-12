# Path to dotfiles repo
export DOTFILES="${${:-$HOME/.zshrc}:A:h}"
if [[ $OSTYPE != darwin* ]]; then
  export BREW_PREFIX=/home/linuxbrew/.linuxbrew
else
  export BREW_PREFIX=/opt/homebrew
fi

# temporary workaround for https://stackoverflow.com/questions/15454174/how-can-a-shell-function-know-if-it-is-running-within-a-virtualenv#comment31587975_15454284
unset VIRTUAL_ENV

# Find all zsh files
configs=($DOTFILES/*/*.(bash|zsh))
configs=(${(M)configs:#*.bash} ${configs:#*.bash})
for file in ${configs:#*/completions.zsh}
do
  source "$file"
done

# Load autocomplete and other zsh stuff
autoload -Uz compinit
zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
if [[ -s $zcompdump && (! -s $zcompdump.zwc || $zcompdump -nt $zcompdump.zwc) ]]; then
  zcompile "$zcompdump"
fi
compinit -C
unset zcompdump

for file in ${(M)configs:#*/completions.zsh}
do
  source "$file"
done

zmodload zsh/stat
typeset -A hist_stat
zstat -H hist_stat "$HISTFILE"

# Cache the byte size and inode alongside the line count so append-only history
# updates only need to scan the new bytes.
old_line_count=0
old_hist_size=0
old_hist_inode=0
if [[ -f ~/.hist_line_count ]]; then
  IFS=' ' read -r old_line_count old_hist_size old_hist_inode < ~/.hist_line_count
fi

if [[ $old_hist_inode == $hist_stat[inode] && $old_hist_size == $hist_stat[size] ]]; then
  line_count=$old_line_count
elif [[ $old_hist_inode == $hist_stat[inode] && $old_hist_size -lt $hist_stat[size] ]]; then
  added_line_count=$(tail -c +$((old_hist_size + 1)) "$HISTFILE" | wc -l)
  line_count=$((old_line_count + added_line_count))
else
  line_count=$(wc -l < "$HISTFILE")
fi

if [[ $line_count -lt 1000 ]]; then
  echo "warning: ~/.keith_zsh_history looks borked"
fi

# -some to give some buffer as history isn't always written immediately from old terminals it seems
if [[ $line_count -lt $((old_line_count - 1000)) ]]; then
  echo "warning: history count just went down, was it truncated? Went from $old_line_count to $line_count"
fi

echo "$line_count $hist_stat[size] $hist_stat[inode]" > ~/.hist_line_count
unset added_line_count hist_stat old_hist_inode old_hist_size old_line_count

if [[ -n "${ALACRITTY_WINDOW_ID:-}" ]]; then
  set-alacritty-theme &!
fi
