# Enable aliases to be sudo’ed
alias sudo="sudo "

# Easier navigation
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias :q="exit"

# Shortcuts
alias g="git"
alias gs="g s"
alias Gs="g s"
alias gp="g p"
alias gc="g c"
alias ga="g a"
alias gap="g ap"
alias gd="g d"
alias gdt="g dt"
alias gdc="g dc"
alias gba="g ba"
alias grv="g rv"
alias gsub="g sub"
alias gl="g l"

# Vim
alias vundle="vim +BundleClean +BundleInstall +qall"
alias v="vim"
alias vv="vim ."

# Archives
alias mktar="tar -pvczf"
alias untar="tar -zxvf"

# File size
alias fs="stat -f %z"

# SSH configuration
alias linode="ssh ksmiley@66.175.208.254"

# Winthrop
alias wl="ssh acc.smileyk2@tampa.winthrop.edu"
alias wa="ssh acc.smileyk2@reno.winthrop.edu"

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
  colorflag="--color"
else # OS X `ls`
  colorflag="-G"
fi

# List all files colorized in long format
alias l="ls -l ${colorflag}"

# List all files colorized in long format, including dot files
alias la="ls -la ${colorflag}"

# Always use color output for `ls`
alias ls="command ls ${colorflag}"
export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'

# Scripts
alias t="python $DOTFILES/scripts/t/t.py --task-dir $HOME/Dropbox/tasks --list tasks"
alias ct="clear; t"

