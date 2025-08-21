#!/bin/bash

# Install the static glibc binary of datadog:$DATAGOG_RELEASE

set -euo pipefail

DATAGOG_RELEASE="v3.4.0"

# Detect architecture
ARCH=$(uname -m)
case "$ARCH" in
x86_64)
  PLATFORM="x64"
  EXPECTED_SHA256="94de024fe9826e7a4023cfa3ad60f3cf08310ffb2c061c2703a025bbb2532c5f"
  ;;
aarch64)
  PLATFORM="arm64"
  EXPECTED_SHA256="b90082a9f0a363c17cb0945b4c26745c07d6756ff0239623b8074c5f35d65f45"
  ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

# Define file path
TARGET_PATH="/usr/local/bin/datadog-ci"
TMP_FILE="$(mktemp)"

# Download datadog-ci
URL="https://github.com/DataDog/datadog-ci/releases/download/$DATAGOG_RELEASE/datadog-ci_linux-$PLATFORM"
echo "Downloading datadog-ci for $PLATFORM $URL..."

curl -L -Ss --fail "$URL" -o "$TMP_FILE"

DOWNLOADED_SHA256=$(sha256sum "$TMP_FILE" | cut -d ' ' -f1)
if [[ "$DOWNLOADED_SHA256" != "$EXPECTED_SHA256" ]]; then
  echo "SHA256 mismatch!"
  echo "Expected: $EXPECTED_SHA256"
  echo "Got:      $DOWNLOADED_SHA256"
  rm -f "$TMP_FILE"
  exit 1
fi

mv "$TMP_FILE" "$TARGET_PATH"
chmod +x "$TARGET_PATH"

echo "datadog-ci installed successfully to $TARGET_PATH"

/usr/local/bin/datadog-ci --version
