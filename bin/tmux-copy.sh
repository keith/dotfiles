#!/bin/bash

set -euo pipefail

if command -v pbcopy >/dev/null; then
  exec reattach-to-user-namespace pbcopy
else
  exec xclip -selection c
fi
