#!/usr/bin/env bash

export ANDROID_HOME="$HOME/adt/sdk"

if [[ -d $ANDROID_HOME ]];then
    export PATH="$ANDROID_HOME/tools:$PATH"
    export PATH="$ANDROID_HOME/platform-tools:$PATH"
fi
