#!/usr/bin/env bash

set -e

qldirectory=~/Library/QuickLook

PLUGINS=( \
  whomwah/qlstephen \
  Keithbsmiley/PlaygroundQuickLook \
)

mkdir -p "$qldirectory"
for PLUGIN in ${PLUGINS[@]}
do
  filename=$(ghb download-release -f -r $PLUGIN)
  extract "$filename"
done

qlgenerators=(*.qlgenerator)
for qlgenerator in ${qlgenerators[@]}
do
  rm -rf "$qldirectory/$qlgenerator"
  mv "$qlgenerator" "$qldirectory"
done

rm release.*
