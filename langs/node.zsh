if [[ -s "$NVM_DIR/nvm.sh" ]]; then
  _nvm_script="$NVM_DIR/nvm.sh"
elif [[ -s "$BREW_PREFIX/opt/nvm/nvm.sh" ]]; then
  _nvm_script="$BREW_PREFIX/opt/nvm/nvm.sh"
else
  return
fi

_nvm_load() {
  unset -f nvm 2>/dev/null
  source "$_nvm_script" "$@"
  local load_result=$?
  unset _nvm_script
  unset -f _nvm_load
  return $load_result
}

# NVM spends most of shell startup resolving and activating its default alias.
# Resolve local aliases directly so Node is immediately available, then load
# NVM's shell functions only when the nvm command is first used.
nvm_version=default
if [[ ! -r "$NVM_DIR/alias/default" ]]; then
  nvm_version=system
fi
nvm_seen_versions=:
while [[ -r "$NVM_DIR/alias/$nvm_version" ]]; do
  if [[ $nvm_seen_versions == *":$nvm_version:"* ]]; then
    nvm_version=
    break
  fi
  nvm_seen_versions+="$nvm_version:"
  nvm_version=$(< "$NVM_DIR/alias/$nvm_version")
done
nvm_version=${nvm_version#v}

nvm_version_dirs=()
if [[ -n $nvm_version && $nvm_version != *[^0-9.]* ]]; then
  nvm_version_dirs=("$NVM_DIR"/versions/node/v${nvm_version}(|.*)(Nn/))
fi
nvm_version_dir=$nvm_version_dirs[-1]

if [[ $nvm_version == system ]]; then
  nvm() {
    _nvm_load --no-use && nvm "$@"
  }
elif [[ -x "$nvm_version_dir/bin/node" && ${NVM_SYMLINK_CURRENT:-false} != true && -z ${PREFIX:-} ]]; then
  export NVM_BIN="$nvm_version_dir/bin"
  export NVM_INC="$nvm_version_dir/include/node"
  export PATH="$NVM_BIN:$PATH"
  hash -r

  nvm() {
    _nvm_load --no-use && nvm "$@"
  }
else
  _nvm_load
fi

unset nvm_seen_versions nvm_version nvm_version_dir nvm_version_dirs
