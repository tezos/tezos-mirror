#!/bin/bash

# Install the static glibc binary of sccache:$SCCACHE_RELEASE

set -euo pipefail

SCCACHE_RELEASE="${SCCACHE_RELEASE:-v0.8.1}"

# Detect architecture
ARCH=$(uname -m)
case "$ARCH" in
x86_64)
  PLATFORM="x86_64"
  EXPECTED_SHA256="e0ee621fb16b6940666cd770b091c62cadafd3e062dd12e3a49d9caaff3b795f"
  ;;
aarch64)
  PLATFORM="aarch64"
  EXPECTED_SHA256="452cef732b24415493a7c6bca6e13536eb9464593fa87c753b6b7cb4733e9c50"
  ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

# Define file path
TARGET_PATH="/usr/local/bin/sccache"
TMP_FILE="$(mktemp)"

# Download sccache
echo "Downloading sccache-$SCCACHE_RELEASE for $PLATFORM..."
curl -L -o "$TMP_FILE" "https://github.com/mozilla/sccache/releases/download/$SCCACHE_RELEASE/sccache-$SCCACHE_RELEASE-$ARCH-unknown-linux-musl.tar.gz"

DOWNLOADED_SHA256=$(sha256sum "$TMP_FILE" | cut -d ' ' -f1)
if [[ "$DOWNLOADED_SHA256" != "$EXPECTED_SHA256" ]]; then
  echo "SHA256 mismatch!"
  echo "Expected: $EXPECTED_SHA256"
  echo "Got:      $DOWNLOADED_SHA256"
  rm -f "$TMP_FILE"
  exit 1
fi

tar zxvf "$TMP_FILE"
cp "sccache-$SCCACHE_RELEASE-$ARCH-unknown-linux-musl/sccache" "$TARGET_PATH"
chmod +x "$TARGET_PATH"
rm -Rf "sccache-$SCCACHE_RELEASE-$ARCH-*"

echo "sccache installed successfully to $TARGET_PATH"
