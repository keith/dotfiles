#!/bin/bash

set -euo pipefail

readonly jdk_version=21.0.10
readonly zulu_version=21.48.17
readonly major_java_version=21

if [[ "$OSTYPE" == darwin* ]]; then
  arch="$(uname -m)"
  if [[ "$arch" == arm64 ]]; then
    readonly jdk_file_sha256="7c29d3232a163c366af3fda303bd54ffda22f41c16ba4c87f19b6adb91b1d232"
    readonly jdk_file_url="https://cdn.azul.com/zulu/bin/zulu$zulu_version-ca-jdk$jdk_version-macosx_aarch64.tar.gz"
  else
    readonly jdk_file_sha256="0dc3de9c4de5fe732a09bafe2103687c365c904ab9542043e89c829dd49e9e28"
    readonly jdk_file_url="https://cdn.azul.com/zulu/bin/zulu$zulu_version-ca-jdk$jdk_version-macosx_x64.tar.gz"
  fi
elif [[ "$OSTYPE" == linux-gnu ]]; then
  readonly jdk_file_sha256="d9bcbea873e758c1282cccee2f73a0e733d23f054fc1b0f6a876169e73a28076"
  readonly jdk_file_url="https://cdn.azul.com/zulu/bin/zulu$zulu_version-ca-jdk$jdk_version-linux_x64.tar.gz"
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
if [[ "$OSTYPE" == darwin* ]]; then
  custom_java_home="$root/zulu-$major_java_version.jdk/Contents/Home"
fi

JAVA_HOME="$custom_java_home" "$@"
