#!/bin/bash

set -euo pipefail

# https://stackoverflow.com/a/21106592/902968
exec 6>&2
exec 2> /tmp/android-stderr.log
set -x

readonly android_sdk_version="14742923"
readonly android_sdk_license_hash="24333f8a63b6825ea9c5514f83c2829b004d1fee"

if [[ "$OSTYPE" == darwin* ]]; then
  readonly android_sdk_file_url="https://dl.google.com/android/repository/commandlinetools-mac-${android_sdk_version}_latest.zip"
  readonly android_sdk_file_sha256="ed304c5ede3718541e4f978e4ae870a4d853db74af6c16d920588d48523b9dee"
elif [[ "$OSTYPE" == linux-gnu ]]; then
  readonly android_sdk_file_url="https://dl.google.com/android/repository/commandlinetools-linux-${android_sdk_version}_latest.zip"
  readonly android_sdk_file_sha256="04453066b540409d975c676d781da1477479dde3761310f1a7eb92a1dfb15af7"
else
  echo "error: Android SDK setup doesn't support this OS: $OSTYPE" >&2
  exit 1
fi

readonly android_sdk_root_dir="/tmp/androidbin/sdk"
readonly android_sdk_unarchived_dir="$android_sdk_root_dir/android-sdk-$android_sdk_version-unarchived"

if [[ -n "${OLD_NDK:-}" ]]; then
  readonly ndk_version="21.3.6528147"
else
  readonly ndk_version="29.0.14206865"
fi

readonly sdkmanager="$android_sdk_unarchived_dir/cmdline-tools/latest/bin/sdkmanager"
readonly install_android_sdk_packages_command=(
  "$sdkmanager"
  "--install"
  "platform-tools"
  "ndk;$ndk_version"
  "platforms;android-35"
  "build-tools;35.0.0"
)

install_android_sdk_packages_command_sha256=$(echo "${install_android_sdk_packages_command[@]}" | shasum -a 256 | cut -d " " -f1)
readonly android_sdk_packages_cache_file="$android_sdk_unarchived_dir/$install_android_sdk_packages_command_sha256-installed-marker"

if [[ ! -f "$android_sdk_packages_cache_file" ]]; then
  rm -rf "$android_sdk_root_dir"
  mkdir -p "$android_sdk_unarchived_dir"

  file="$(mktemp)"
  curl -o "$file" --silent --fail "$android_sdk_file_url"
  if ! echo "$android_sdk_file_sha256  $file" | shasum --check --status; then
    echo "error: android SDK download sha mismatch" >&2
    exit 1
  fi

  unzip -q "$file" -d "$android_sdk_unarchived_dir"
  rm -f "$file"

  # The zip extracts to cmdline-tools/; sdkmanager expects it at cmdline-tools/latest/
  mv "$android_sdk_unarchived_dir/cmdline-tools" "$android_sdk_unarchived_dir/cmdline-tools-tmp"
  mkdir -p "$android_sdk_unarchived_dir/cmdline-tools"
  mv "$android_sdk_unarchived_dir/cmdline-tools-tmp" "$android_sdk_unarchived_dir/cmdline-tools/latest"

  mkdir -p "$android_sdk_unarchived_dir/licenses"
  echo "$android_sdk_license_hash" > "$android_sdk_unarchived_dir/licenses/android-sdk-license"

  script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
  ANDROID_HOME="$android_sdk_unarchived_dir" "$script_dir/jdk-wrapper.sh" "${install_android_sdk_packages_command[@]}" | (grep -v = || true)

  touch "$android_sdk_packages_cache_file"
fi

# See exec uses above
exec 2>&6 6>&-

ANDROID_HOME=$android_sdk_unarchived_dir \
  ANDROID_NDK_HOME="$android_sdk_unarchived_dir/ndk/$ndk_version/" \
  PATH="$android_sdk_unarchived_dir/cmdline-tools/latest/bin:$android_sdk_unarchived_dir/platform-tools:$PATH" \
  "$@"
