#!/usr/bin/env bash

linkdir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
keybinddir=~/Library/KeyBindings
mkdir -p $keybinddir
ln -s "$linkdir/*.dict" $keybinddir
