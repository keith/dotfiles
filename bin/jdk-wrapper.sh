#!/bin/bash

set -euo pipefail

readonly jdk_version=8.0.302
readonly major_java_version=8

if [[ "$OSTYPE" == darwin* ]]; then
  arch="$(uname -m)"
  if [[  "$arch" == arm64 ]]; then
    readonly jdk_file_sha256="4482990c96e87519f52725b0bf3a6171510e3da268d55b793d1bf6eeb6485030"
    readonly jdk_file_url="https://cdn.azul.com/zulu/bin/zulu8.56.0.23-ca-jdk$jdk_version-macosx_aarch64.tar.gz"
  else
    readonly jdk_file_sha256="497c1d6eae5f3943a1c5f74be7bb8a650d6b0dc3bf069973d6d04f45c3daaf88"
    readonly jdk_file_url="https://cdn.azul.com/zulu/bin/zulu8.56.0.21-ca-jdk$jdk_version-macosx_x64.tar.gz"
  fi
elif [[ "$OSTYPE" == linux-gnu ]]; then
  readonly jdk_file_sha256="f6e6946713575aeeadfb75bd2eb245669e59ce4f797880490beb53a6c5b7138a"
  readonly jdk_file_url="https://cdn.azul.com/zulu/bin/zulu8.56.0.21-ca-jdk$jdk_version-linux_x64.tar.gz"
else
  echo "error: JDK $major_java_version setup doesn't support this OS: $OSTYPE" >&2
  exit 1
fi

readonly root="/tmp/keith-jdk/$jdk_version"
readonly marker_file="$root/marker-$jdk_file_sha256"

if [[ ! -f "$marker_file" ]]; then
  archive="$(mktemp)"
  curl -o "$archive" --silent --fail "$jdk_file_url"
  if ! echo "$jdk_file_sha256  $archive" | shasum --check --status; then
    echo "error: jdk download sha mismatch" >&2
    exit 1
  fi

  rm -rf "$root"
  mkdir -p "$root"
  tar -xf "$archive" -C "$root" --strip-components=1
  touch "$marker_file"
  rm -f "$archive"
fi

custom_java_home="$root"
if [[ "$OSTYPE" == "darwin"* ]]; then
  custom_java_home="$root/zulu-$major_java_version.jdk/Contents/Home"
fi

JAVA_HOME="$custom_java_home" "$@"
