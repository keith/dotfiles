#!/bin/bash

set -euo pipefail
set -x

# https://stackoverflow.com/a/21106592/902968
exec 6>&2
exec 2> "/tmp/android-stderr.log"

readonly android_sdk_version="4333796"
readonly android_sdk_license_hash="24333f8a63b6825ea9c5514f83c2829b004d1fee"

if [[ "$OSTYPE" == darwin* ]]; then
  readonly android_sdk_file_url="https://dl.google.com/android/repository/sdk-tools-darwin-$android_sdk_version.zip"
  readonly android_sdk_file_sha256="ecb29358bc0f13d7c2fa0f9290135a5b608e38434aad9bf7067d0252c160853e"
elif [[ "$OSTYPE" == linux-gnu ]]; then
  readonly android_sdk_file_url="https://dl.google.com/android/repository/sdk-tools-linux-$android_sdk_version.zip"
  readonly android_sdk_file_sha256="92ffee5a1d98d856634e8b71132e8a95d96c83a63fde1099be3d86df3106def9"
else
  echo "error: Android SDK setup doesn't support this OS: $OSTYPE" >&2
  exit 1
fi

readonly android_sdk_root_dir="/tmp/androidbin/sdk"
readonly android_sdk_unarchived_dir="$android_sdk_root_dir/android-sdk-$android_sdk_version-unarchived"

readonly cmdline_tools_version="7.0"
readonly ndk_version="21.3.6528147"
readonly install_android_cmd_line_tools=(
  "$android_sdk_unarchived_dir/tools/bin/sdkmanager"
  "--install"
  "cmdline-tools;$cmdline_tools_version"
)
readonly install_android_sdk_packages_command=(
  "$android_sdk_unarchived_dir/cmdline-tools/$cmdline_tools_version/bin/sdkmanager"
  "--install"
  "platform-tools"
  "ndk;$ndk_version"
  "platforms;android-30"
  "build-tools;30.0.2"
)

readonly install_cmd=("${install_android_cmd_line_tools[@]}" "${install_android_sdk_packages_command[@]}")
install_android_sdk_packages_command_sha256=$(echo "${install_cmd[@]}" | shasum -a 256 | cut -d " " -f1)
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

  mkdir -p "$android_sdk_unarchived_dir/licenses"
  echo "$android_sdk_license_hash" > "$android_sdk_unarchived_dir/licenses/android-sdk-license"

  script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
  ANDROID_HOME="$android_sdk_unarchived_dir" "$script_dir/jdk-wrapper.sh" "${install_android_cmd_line_tools[@]}" | (grep -v = || true)
  ANDROID_HOME="$android_sdk_unarchived_dir" "$script_dir/jdk-wrapper.sh" "${install_android_sdk_packages_command[@]}" | (grep -v = || true)

  touch "$android_sdk_packages_cache_file"
fi

# See exec uses above
exec 2>&6 6>&-

ANDROID_HOME=$android_sdk_unarchived_dir \
  ANDROID_NDK_HOME="$android_sdk_unarchived_dir/ndk/$ndk_version/" \
  PATH="$android_sdk_unarchived_dir/tools/bin:$android_sdk_unarchived_dir/platform-tools:$PATH" \
  "$@"
