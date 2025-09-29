#!/bin/bash

# Install the static glibc binary of datadog:$DATAGOG_RELEASE

set -euo pipefail

DATAGOG_RELEASE="v3.20.0"

# Detect architecture
ARCH=$(uname -m)
case "$ARCH" in
x86_64)
  PLATFORM="x64"
  EXPECTED_SHA256="e238975dca030800c4c504f94f02faccc65d84e11356e8de34165a5e7041f352"
  ;;
aarch64)
  PLATFORM="arm64"
  EXPECTED_SHA256="6a68bc884f1f2f19b4fc77a81dd08836f78bb6e558675836f8bf91574833affa"
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

if command -v kiss-fetch.sh > /dev/null 2>&1; then
  kiss-fetch.sh "$URL" -o "$TMP_FILE"
else
  echo "Warning: Kiss-fetch.sh missing"
  curl -L -Ss --fail "$URL" -o "$TMP_FILE"
fi

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
