#!/usr/bin/env bash
# This is a script for bootstrapping OS X setup

if ! which xcodebuild &> /dev/null; then
    echo "You need to install the Xcode Command Line Tools before running this script"
    exit
fi

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

if [[ ! -e "manage.sh" ]]; then
    echo "Make sure you have the manage script in the same directory!"
    exit
fi

cd $(dirname $0)
cd ..
./manage.sh install

if [[ ! -e "$HOME/.bashrc" ]]; then
    echo "Looks like the manage script failed, try and run it manually"
    exit
fi

source "$HOME/.bashrc"

if ! which brew &> /dev/null; then
    echo "Homebrew is not installed in your \$PATH"
    exit
fi

brew tap homebrew/dupes
brew update

formulas=(bash cloc ctags git grc grep heroku-toolbelt hub imagemagick less llvm macvim mercurial mogenerator "mutt --with-sidebar-patch" node openssh openssl python python3 readline reattach-to-user-namespace sqlite the_silver_searcher tmux tree valgrind vim wget zsh zsh-completions)

for f in ${formulas[@]}
do
  brew install $f
done

curl -s -O http://github-media-downloads.s3.amazonaws.com/osx/git-credential-osxkeychain
chmod u+x git-credential-osxkeychain
sudo mv git-credential-osxkeychain "$(dirname $(which git))/git-credential-osxkeychain"

./install-langs.sh install

