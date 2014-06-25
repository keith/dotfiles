#!/usr/bin/env bash

qldirectory=~/Library/QuickLook

PLUGINS=( \
    https://github.com/whomwah/qlstephen/releases/download/1.4.2/QLStephen.qlgenerator.1.4.2.zip \
    https://github.com/Keithbsmiley/PlaygroundQuickLook/releases/download/0.1.0/PlaygroundQuickLook.qlgenerator.zip \
)

mkdir -p "$qldirectory"
for PLUGIN in ${PLUGINS[@]}
do
    wget "$PLUGIN"
done

unzip \*.zip
qlgenerators=(*.qlgenerator)
for qlgenerator in ${qlgenerators[@]}
do
    rm -rf "$qldirectory/$qlgenerator"
    mv "$qlgenerator" "$qldirectory"
done

rm *.zip
