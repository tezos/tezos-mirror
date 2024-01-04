#!/bin/sh
set -e

if [ -z "$EXECUTABLE_FILES" ]; then
  echo "Error: environment variable EXECUTABLE_FILES is empty."
  echo "Set it to e.g. 'script-inputs/released-executables'"
  echo "or to 'script-inputs/released-executables script-inputs/experimental-executables'."
  exit 1
fi

if [ -z "$ARCH" ]; then
  echo "Error: environment variable ARCH is empty."
  echo "Set it to e.g. 'x86_64' or 'amd64'."
  exit 1
fi

set -u

echo "Create destination directory"
mkdir -pv "octez-binaries/$ARCH"

echo "Build and install static binaries"
# shellcheck disable=SC2086
make static OCTEZ_EXECUTABLES="$(cat $EXECUTABLE_FILES)"

echo "Check executables and move them to the destination directory"
# Disable https://www.shellcheck.net/wiki/SC2086 because:
# - it's meant to prevent word splitting and we actually want to split words
#   in $EXECUTABLE_FILES and we can't use an array because the variable
#   comes from the CI
# - it's meant to prevent globbing but actually we don't mind globbing here
#   even if we don't use it right now
# shellcheck disable=SC2086
cat $EXECUTABLE_FILES |
  while read -r executable; do
    if [ ! -f "$executable" ]; then
      echo "Error: $executable does not exist"
      exit 1
    fi

    if (file "$executable" | grep -q "statically linked"); then
      echo "Statically linked: $executable"
    else
      echo "Error: $executable is not statically linked"
      exit 1
    fi

    mv "$executable" "octez-binaries/$ARCH/$executable"
    # Write access is needed by strip below.
    chmod +w "octez-binaries/$ARCH/$executable"
  done

echo 'Check octez-client --version'
SHA=$(git rev-parse --short=8 HEAD)
client_version=$("octez-binaries/$ARCH/octez-client" --version | cut -f 1 -d ' ')
if [ "$SHA" != "$client_version" ]; then
  echo "Unexpected version for octez-client (expected $SHA, found $client_version)"
  exit 1
fi
echo "octez-client --version returned the expected commit hash: $SHA"

echo "Strip debug symbols and compress binaries (parallelized)"
# shellcheck disable=SC2046,SC2038
find "octez-binaries/$ARCH" -maxdepth 1 -type f ! -name "*.*" | xargs -n1 -P$(nproc) -i sh -c 'strip --strip-debug {}; upx -6q {};'

# Show the effect of previous actions
find "octez-binaries/$ARCH" -maxdepth 1 -type f ! -name "*.*" |
  while read -r b; do
    file "$(realpath "$b")"
  done
