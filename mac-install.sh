#!/usr/bin/env bash
# This is an ugly script for bootstrapping OS X setup

if ! which xcodebuild &> /dev/null; then
    echo "You need to install the Xcode Command Line Tools before running this script"
    exit
fi

ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)" &
wait

if [[ ! -e "manage.sh" ]]; then
    echo "Make sure you have the manage script in the same directory!"
    exit
fi

./manage.sh install &
rm -rf $HOME/.rbenv
wait

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
brew tap homebrew/versions
brew tap phinze/homebrew-cask
brew update

brew install apple-gcc42 appledoc autoconf automake bash boost brew-cask bsdmake cabextract class-dump clisp cloc cmake ctags doxygen dsniff flac fontconfig freetype gcc gdbm gettext git glew glib glm gmp gnu-sed gnu-tar gnu-which gnustep-make gnutls gource gptfdisk grep heroku-toolbelt hub icu4c imagemagick jasper jbig2dec jpeg lame libevent libffi libgpg-error libicns libksba libmikmod libmpc libnet libnids libogg libpng libsigsegv libtasn1 libtiff libtool libvorbis little-cms little-cms2 llvm lynx macvim markdown mercurial mogenerator mongodb mpfr mpg123 mysql neon nettle ngrep nmap node openssh openssl ossp-uuid p11-kit pcre pidof pkg-config popt postgresql proctools pyqt python python3 readline reattach-to-user-namespace rsync ruby-build scrub sdl sdl_gfx sdl_image sdl_mixer sdl_ttf sendemail serf sip sqlite the_silver_searcher tmux todo-txt tor tree valgrind vim wget xctool xz zsh zsh-completions  

git clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
git clone https://github.com/sstephenson/rbenv-default-gems.git $HOME/.rbenv/plugins/rbenv-default-gems
git clone git://github.com/sstephenson/ruby-build.git $HOME/.rbenv/plugins/ruby-build
git clone https://github.com/sstephenson/rbenv-gem-rehash.git $HOME/.rbenv/plugins/rbenv-gem-rehash
git clone git://github.com/tpope/rbenv-readline.git $HOME/.rbenv/plugins/rbenv-readline

./manage.sh install &
wait

