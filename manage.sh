#!/usr/bin/env bash

FILES=(\
    agignore \
    amethyst \
    backupignore \
    bashrc \
    bin \
    curlrc \
    emacs.d \
    gemrc \
    ghci \
    gitattributes \
    gitconfig \
    gitignore_global \
    git_template \
    gnupg \
    goobookrc \
    gvimrc \
    hgrc \
    haskeline \
    hushlogin \
    hydra \
    ideavimrc \
    inputrc \
    irbrc \
    irssi \
    mutt \
    notmuch-config \
    npmrc \
    offlineimaprc \
    psqlrc \
    pylintrc \
    rspec \
    shellrc \
    slate.js \
    tm_properties \
    tmux-powerlinerc \
    tmux.conf \
    urlview \
    vim \
    vimrc \
    Xdefaults \
    Xmodmap \
    xvimrc \
    zshrc \
)
LINUX=(Xdefaults Xmodmap)
NO_DOT=()

function custom_path () {
    for i in "${!PATHS[@]}"
    do
        if [[ $1 == $i ]]; then
            return 0
        fi
    done

    return 1
}

function no_dot () {
    local filename=$1
    for FILE in ${NO_DOT[@]}
    do
        if [[ $FILE == $filename ]]; then
            return 0
        fi
    done

    return 1
}

function new_path () {
    local filename=$1

    if ! no_dot $filename; then
        filename=".$filename"
    fi

    echo "$HOME/$filename"
}

# Links the passed filename to its new location
function link () {
    local filename=$1

    if [[ ! -e $filename ]]; then
        echo "$filename doesn't exist"
        return
    fi

    if is_osx; then
        for FILE in ${LINUX[@]}
        do
            if [[ $FILE == $filename ]]; then
                return
            fi
        done
    fi

    local path=$(new_path $filename)
    if [[ ! -e $path ]]; then
        echo "Linking $filename to $path"
        ln -s $PWD/$filename $path
    fi
}

# Delete the linked file path
function unlink () {
    local filename=$1
    local path=$(new_path $filename)

    if [ -e $path ]; then
        rm $(new_path $1)
        echo "Removing $(new_path $1)"
    fi
}

# Loops through and link all files without links
function install_links () {
    for FILE in ${FILES[@]}
    do
        link $FILE
    done
}

# Function to remove all linked files
function remove_links () {
    for FILE in ${FILES[@]}
    do
        unlink $FILE
    done
}

function is_osx () {
    if [[ $OSTYPE == darwin* ]];then
        return 0
    else
        return 1
    fi
}

# Fuction to print the usage and exit when there's bad input
function die () {
    echo "Usage ./manage.sh {install|remove}"
    exit
}

# Make sure there is 1 command line argument
if [[ $# != 1 ]]; then
    die
fi

# Check whether the user is installing or removing
if [[ $1 == "install" ]]; then
    install_links
    # It's required for this to have these permissions
    chmod 0600 ~/.mutt/msmtprc
elif [[ $1 == "remove" ]]; then
    remove_links
else
    die
fi
