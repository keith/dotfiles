if [[ "$OSX" != true ]]; then
  return
fi

GNU_MANPATHS=( \
  /usr/local/opt/coreutils/libexec/gnuman \
  /usr/local/opt/findutils/libexec/gnuman \
  /usr/local/opt/gnu-sed/libexec/gnuman   \
  /usr/local/opt/gnu-tar/libexec/gnuman   \
)

GNU_BINS=( \
  /usr/local/opt/coreutils/libexec/gnubin \
  /usr/local/opt/findutils/libexec/gnubin \
  /usr/local/opt/gnu-sed/libexec/gnubin   \
  /usr/local/opt/gnu-tar/libexec/gnubin   \
)

NEW_MANPATH=""
for GNU_MANPATH in "${GNU_MANPATHS[@]}"
do
  NEW_MANPATH="$GNU_MANPATH:$NEW_MANPATH"
done

NEW_PATH="$PATH"
for GNU_BIN in "${GNU_BINS[@]}"
do
  NEW_PATH="$GNU_BIN:$NEW_PATH"
done

export MANPATH="$NEW_MANPATH"
export PATH="$NEW_PATH"
