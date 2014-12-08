#!/usr/bin/env bash

FILES=(\
    agignore \
    bashrc \
    bin \
    ctags \
    curlrc \
    gemrc \
    ghci \
    git_template \
    gitattributes \
    gitconfig \
    gitignore \
    haskeline \
    hgrc \
    hushlogin \
    ideavimrc \
    inputrc \
    irbrc \
    js \
    lldbinit \
    mutt \
    notmuch-config \
    npmrc \
    nvim \
    nvimrc \
    offlineimaprc \
    phoenix.js \
    psqlrc \
    pylintrc \
    rbenv \
    rspec \
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

function custom_path () {
    for i in "${!PATHS[@]}"
    do
        if [[ $1 == "$i" ]]; then
            return 0
        fi
    done

    return 1
}

function new_path () {
    echo "$HOME/.$1"
}

# Links the passed filename to its new location
function link () {
    local filename=$1

    if [[ ! -e $filename ]]; then
        echo "$filename doesn't exist"
        return
    fi

    if is_osx; then
        for FILE in "${LINUX[@]}"
        do
            if [[ $FILE == "$filename" ]]; then
                return
            fi
        done
    fi

    local path=$(new_path "$filename")
    if [[ ! -e "$path" ]]; then
        echo "Linking $filename to $path"
        ln -s "$PWD/$filename" "$path"
    fi
}

# Delete the linked file path
function unlink () {
    local filename=$1
    local path=$(new_path "$filename")

    if [ -e "$path" ]; then
        rm "$(new_path "$1")"
        echo "Removing $(new_path "$1")"
    fi
}

# Loops through and link all files without links
function install_links () {
    for FILE in "${FILES[@]}"
    do
        link $FILE
    done
}

# Function to remove all linked files
function remove_links () {
    for FILE in "${FILES[@]}"
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
