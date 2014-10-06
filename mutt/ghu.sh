#!/bin/sh
#
# pbrisbin 2014
# https://github.com/pbrisbin/ghu.git
#
###
unsubscribe=$(sed "/^List-Unsubscribe: /,/^[A-Z].*: /!d
  #
  # Result:
  #
  #   List-Unsubscribe: <mailto:...>,
  #    <https://...>
  #   X-Whatever: ...
  #

s/^[A-Z].*: //g
  #
  # Result:
  #
  #   <mailto:...>,
  #    <https://...>
  #   ...
  #

/http/!d
  #
  # Result:
  #
  #    <https://...>
  #

s/^ *<\([^>]*\)> *$/\1/g
  #
  # Result:
  #
  #   https:...
  #
")

[ -n "$unsubscribe" ] || exit 1

curl -# "$unsubscribe" >/dev/null && printf "OK\n"
