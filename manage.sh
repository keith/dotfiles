#!/usr/bin/env bash

FILES=(bashrc bin gemrc gitignore_global gvimrc hgrc inputrc irbrc irssi mutt muttrc rspec slate tm_properties tmux.conf tmux-powerlinerc vim vimrc xvimrc zshrc)
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
    else
        echo "$path doesn't exist"
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

function install_gitconfig () {
    local filename="gitconfig"
    local prefix="linux"
    if is_osx;then
        prefix="osx"
    fi

    if [ ! -e $HOME/.$filename ];then
        echo "Linking $PWD/$prefix/$filename to $HOME/.$filename"
        ln -s $PWD/$prefix/$filename $HOME/.$filename
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
    git submodule update --init --recursive &
    wait
    install_links
    install_gitconfig
elif [[ $1 == "remove" ]]; then
    remove_links
else
    die
fi

