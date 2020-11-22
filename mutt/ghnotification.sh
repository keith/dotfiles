#!/usr/bin/env bash

beacon_re='https://github\.com/notifications/beacon/.*\.gif'
beacon_url="$(cat | grep -o "$beacon_re")"
[ -n "$beacon_url" ] && curl --silent --max-time 3 "$beacon_url" >/dev/null &
