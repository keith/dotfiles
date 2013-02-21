#!/usr/bin/env bash

FILES=(aliases default-gems gemrc gitconfig gitignore_global hgrc slate tm_properties zshrc)

declare -A PATHS
PATHS=(["default-gems"]=".rbenv")

NO_DOT=(default-gems)
VIM_FILES=(gvimrc vim vimrc)


function custom_path ()
{
	for i in "${!PATHS[@]}"
	do
		# echo $i
		if [[ $1 == $i ]]; then
			return 0
		fi
	done
	
	return 1
}

function no_dot? ()
{
	local filename=$1
	for FILE in ${NO_DOT[@]}
	do
		if [[ $FILE == $filename ]]; then
			return 0
		fi
		# echo $FILE
	done
	
	return 1
}

function new_path ()
{
	local filename=$1
	
	local path=""
	if custom_path $filename; then
		path="${PATHS[$filename]}/"
	fi
	
	if ! no_dot? $filename; then
		filename=".$filename"
		# echo "no dot $filename"
	fi
	
	echo "$HOME/$path$filename"
}

# Links the passed filename to its new location
function link ()
{
	local filename=$1
	
	if [ ! -f $filename ] && [ ! -d $filename ];
	then
	    echo "$filename doesn't exist"
		return
	fi
	
	local path=$(new_path $filename)
	if [ -f $path ] || [ -d $path ]; then
		echo "$path already exists"
	else
		echo "$path needs linking"
	fi
		
	# echo $path
}

# Loops through and link all files without links
function install_links ()
{
	echo "installing"
	for FILE in ${FILES[@]}
	do
		# echo $FILE
		# echo "$HOME/$FILE"
		# a=$(new_path $FILE)
		link $FILE
		# echo $a
		# if [[ ! -f "$HOME/.$FILE" ]]; then
		# 	echo "File doesnt exist"
		# fi
	done

	cd "vim"

	for FILE in ${VIM_FILES[@]}
	do
		link $FILE
		# a=$(new_path $FILE)
		# echo $a
	done
}

# Function to remove all linked files
function remove_links ()
{
	echo "removing"
}

# Fuction to print the usage and exit when there's bad input
function die ()
{
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
elif [[ $1 == "remove" ]]; then
	remove_links
else
	die
fi
