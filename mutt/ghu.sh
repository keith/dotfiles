#!/bin/sh
#
# pbrisbin 2014
#
###
err_usage() {
  cat >&2 <<EOF
Usage: ghu [options] < file

Parse stdin as an email for any List-Unsubscribe header containing a URL. Visit
that URL using curl, thereby unsubscribing you from the thread.

Options:
  -o, --open    Open the unsubscribe link with \$BROWSER, not curl
  -p, --print   Print the unsubscribe link, do not visit it

EOF

  exit 64
}

parseheader="$(dirname "$0")"/parse-header

get() {
  curl --silent "$@" | sed "
    /^.*\(You've been unsubscribed from the thread\).*$/!d
    s//\1/g"
}

parse() {
  local url_re='https?://[^[:space:]>]+'

  "$parseheader" "List-Unsubscribe" | grep -ioE "$url_re"
}

if [ -n "$1" ]; then
  case "$1" in
    -o|--open)  get() { ${BROWSER:-open} "$@"; } ;;
    -p|--print) get() { printf "%s\n" "$*"; } ;;
    *) err_usage ;;
  esac
fi

if unsubscribe="$(parse)" && [ -n "$unsubscribe" ]; then
  get "$unsubscribe"
else
  exit 1
fi
