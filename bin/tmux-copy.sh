#!/bin/bash

set -euo pipefail

if command -v reattach-to-user-namespace; then
  exec reattach-to-user-namespace pbcopy
fi
