if [[ ! -d "$HOME/.cargo" ]]; then
  return
fi

source "$HOME/.cargo/env"

rust_src="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export RUST_SRC_PATH="$rust_src"
