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
	
	echo "$PWD/$path$filename"
}

function install_links ()
{

}

for FILE in ${FILES[@]}
do
	# echo $FILE
	# echo "$HOME/$FILE"
	a=$(new_path $FILE)
	echo $a
	# if [[ ! -f "$HOME/.$FILE" ]]; then
	# 	echo "File doesnt exist"
	# fi
done

cd "vim"

for FILE in ${VIM_FILES[@]}
do
	a=$(new_path $FILE)
	echo $a
done
