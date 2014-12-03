#!/usr/bin/env bash

trap ctrl_c INT
function ctrl_c() {
  exit 130
}

gems=$(cat ~/.rbenv/default-gems)
for gem in "${gems[@]}"
do
  gem install "$gem"
done
