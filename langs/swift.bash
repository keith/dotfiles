# Add swiftpm build to path
export PATH=".git/safe/../../.build/debug:$PATH"

export SWIFTENV_ROOT="$HOME/.swiftenv"
export PATH="$SWIFTENV_ROOT/bin:$PATH"

if command -v swiftenv >/dev/null; then
  eval "$(swiftenv init -)"
fi
