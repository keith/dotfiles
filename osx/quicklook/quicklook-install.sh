#!/usr/bin/env bash

set -e

qldirectory=~/Library/QuickLook

PLUGINS=( \
  keith/PlaygroundQuickLook \
)

mkdir -p "$qldirectory"
for PLUGIN in "${PLUGINS[@]}"
do
  filename=$(ghb download-release -f $PLUGIN)
  extract "$filename"
  rm "$filename"
done

qlgenerators=(*.qlgenerator)
for qlgenerator in "${qlgenerators[@]}"
do
  rm -rf "$qldirectory/$qlgenerator"
  mv "$qlgenerator" "$qldirectory"
done

qlmanage -r
