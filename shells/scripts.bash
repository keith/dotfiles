# Path to script files in dotfiles directory
export SCRIPT_PATH="$DOTFILES/scripts"

# Scripts
alias append="$SCRIPT_PATH/fileAppend.rb"
alias cloudfront="$SCRIPT_PATH/cloudFront.rb"
alias methods="$SCRIPT_PATH/methods.rb"
alias mksite="$SCRIPT_PATH/newSite.rb"
alias pf="$SCRIPT_PATH/processfile.rb"
alias rename="$SCRIPT_PATH/imageRenamer.rb"

export _Z_EXCLUDE_DIRS="/Volumes"
source "$SCRIPT_PATH/z/z.sh"
