#!/usr/bin/env bash

FILES=(terminator awesome)
p=$HOME/.config
dir=$DOTFILES/linux/configs

function link() {
    local folder=$1

    if [[ ! -d $dir/$folder ]];then
        echo "$folder doesn't exist"
        return
    fi

    local path=$p
    if [[ ! -d $path/$folder ]];then
        echo "Linking $dir/$folder to $path"
        ln -s $dir/$folder $path
    fi
}

function new_path() {
    local folder=$1
    echo "$p/$folder"
}

function install_links() {
    for FILE in ${FILES[@]};do
        link $FILE
    done
}

if [[ ! -d "$p" ]];then
    mkdir -p $p
fi

install_links

