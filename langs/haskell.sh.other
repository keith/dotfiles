#!/usr/bin/env bash

set -e

ghc_version="7.8.3"
filename="ghc.tar.xz"
cabal_version="1.20.0.3"
cabal_filename="cabal.tar.gz"

wget "http://www.haskell.org/ghc/dist/$ghc_version/ghc-$ghc_version-x86_64-apple-darwin.tar.xz" -O "$filename"

tar xf "$filename"
pushd "ghc-$ghc_version"
./configure
make install
popd

wget "http://hackage.haskell.org/package/cabal-install-$cabal_version/cabal-install-$cabal_version.tar.gz" -O "$cabal_filename"
tar -zxvf "$cabal_filename"
pushd "cabal-install-$cabal_version"
./bootstrap.sh --no-doc
popd

# vim: tw=0
