export SWIFTENV_ROOT="$HOME/.swiftenv"
export PATH="$SWIFTENV_ROOT/bin:$PATH"

if command -v swiftenv >/dev/null; then
  eval "$(swiftenv init -)"
fi
