#!/usr/bin/env bash

message=$(tee /tmp/mutt.txt)

if echo "${message}" | egrep --silent "From: Trello"; then
  if command -v trelloparse >/dev/null; then
    echo "${message}" | trelloparse
    exit
  fi
fi

echo "${message}"
