#!/usr/bin/env bash

FILES=(\
    agignore \
    bashrc \
    bash_profile \
    bin \
    conductor.js \
    config \
    css \
    ctags \
    curlrc \
    default-gems \
    gemrc \
    ghci \
    git_template \
    gitconfig \
    gitignore \
    haskeline \
    hgrc \
    hushlogin \
    ideavimrc \
    inputrc \
    js \
    lldbinit \
    mailcap \
    mutt \
    notmuch-config \
    npmrc \
    nvim \
    nvimrc \
    offlineimaprc \
    psqlrc \
    pylintrc \
    rspec \
    tmux.conf \
    tmuxinator \
    urlview \
    vim \
    vimrc \
    weechat \
    xvimrc \
    zshrc \
)

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

# Fuction to print the usage and exit when there's bad input
function die () {
    echo "Usage ./manage.sh {install|remove|clean}"
    exit 1
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
elif [[ $1 == "clean" ]]; then
    find -L ~ -type l -maxdepth 1 -exec rm -i {} \;
else
    die
fi
