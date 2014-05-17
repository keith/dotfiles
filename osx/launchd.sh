#!/usr/bin/env bash

cd launchd
agents=(*.plist)

for agent in ${agents[@]}
do
    ln -s $PWD/$agent ~/Library/LaunchAgents
    launchctl load ~/Library/LaunchAgents/$agent
done
