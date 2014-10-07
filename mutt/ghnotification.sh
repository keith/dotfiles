#!/usr/bin/env bash

beacon_re='https://github\.com/notifications/beacon/.*\.gif'
beacon_url="$(cat | grep -o "$beacon_re")"
[ -n "$beacon_url" ] && curl --silent "$beacon_url" >/dev/null
