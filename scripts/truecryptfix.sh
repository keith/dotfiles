#!/usr/bin/env bash

libs=("/usr/local/lib/libmacfuse_i32.2.dylib" \
"/usr/local/lib/libosxfuse_i32.2.dylib" \
"/usr/local/lib/libosxfuse_i64.2.dylib" \
"/usr/local/lib/libmacfuse_i64.2.dylib" \
"/usr/local/lib/libosxfuse_i32.la" \
"/usr/local/lib/libosxfuse_i64.la" \
"/usr/local/lib/pkgconfig/osxfuse.pc")

truecrypt="/Applications/TrueCrypt.app/Contents/Resources/Library"

[ ! -d $truecrypt ] && mkdir -p $truecrypt

for lib in "${libs[@]}"
do
    mv $lib "${truecrypt}/" && echo "Moved ${lib} to ${truecrypt}." || echo "Problem moving: ${lib} to ${truecrypt}"
    if [[ -e $lib ]];then
        rm $lib || echo "Problem removing: ${lib}"
    fi
    ln -s "${truecrypt}/$(basename $lib)" ${lib} && echo "Linked ${lib}." || echo "Problem symlinking ${lib}"
done

brew prune
brew doctor

