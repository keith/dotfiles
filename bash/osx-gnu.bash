if [[ "$OSX" != true ]]; then
  return
fi

gnu_manpaths=( \
  "$BREW_PREFIX/opt/coreutils/libexec/gnuman" \
  "$BREW_PREFIX/opt/findutils/libexec/gnuman" \
  "$BREW_PREFIX/opt/gnu-sed/libexec/gnuman" \
)

gnu_bins=( \
  "$BREW_PREFIX/opt/coreutils/libexec/gnubin" \
  "$BREW_PREFIX/opt/findutils/libexec/gnubin" \
  "$BREW_PREFIX/opt/gnu-sed/libexec/gnubin" \
)

new_manpath=""
for gnu_manpath in "${gnu_manpaths[@]}"
do
  new_manpath="$gnu_manpath:$new_manpath"
done

new_path="$PATH"
for gnu_bin in "${gnu_bins[@]}"
do
  new_path="$gnu_bin:$new_path"
done

export MANPATH="$new_manpath"
export PATH="$new_path"
