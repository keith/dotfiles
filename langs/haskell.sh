#!/usr/bin/env bash

verison=7.8.2
filename=ghc.tar.xz

wget http://www.haskell.org/ghc/dist/$version/ghc-$version-x86_64-apple-darwin-mavericks.tar.xz -O $filename

tar xf $filename
cd ghc-$version
./configure

# vim: tw=0
