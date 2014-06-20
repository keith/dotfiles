#!/usr/bin/env bash

set -e

ghc_version="7.8.2"
filename="ghc.tar.xz"

wget "http://www.haskell.org/ghc/dist/$ghc_version/ghc-$ghc_version-x86_64-apple-darwin-mavericks.tar.xz" -O "$filename"

tar xf $filename
cd ghc-$version
./configure

# vim: tw=0
