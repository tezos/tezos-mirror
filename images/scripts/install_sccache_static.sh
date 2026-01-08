#!/bin/bash

# Install the static glibc binary of sccache:$SCCACHE_RELEASE

set -euo pipefail

SCCACHE_RELEASE="${SCCACHE_RELEASE:-v0.10.0}"

# Detect architecture
ARCH=$(uname -m)
case "$ARCH" in
x86_64)
  PLATFORM="x86_64"
  SHA256_MUSL="1fbb35e135660d04a2d5e42b59c7874d39b3deb17de56330b25b713ec59f849b"
  ;;
aarch64)
  PLATFORM="aarch64"
  SHA256_MUSL="d6a1ce4acd02b937cd61bc675a8be029a60f7bc167594c33d75732bbc0a07400"
  ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

TARGET_SUFFIX="unknown-linux-musl"
EXPECTED_SHA256="${SHA256_MUSL}"

# Ensure required tools are available
for tool in curl tar sha256sum; do
  if ! command -v "$tool" > /dev/null 2>&1; then
    echo "Missing required tool: $tool" >&2
    exit 1
  fi
done

# Ensure SCCACHE_RELEASE is defined
if [ -z "$SCCACHE_RELEASE" ]; then
  echo "SCCACHE_RELEASE is not set. Export it first (e.g. export SCCACHE_RELEASE=0.7.7)" >&2
  exit 1
fi

# Set file paths
TARGET_PATH="/usr/local/bin/sccache"
TMP_FILE="$(mktemp)"
ARCHIVE_NAME="sccache-$SCCACHE_RELEASE-$PLATFORM-$TARGET_SUFFIX.tar.gz"
URL="https://github.com/mozilla/sccache/releases/download/$SCCACHE_RELEASE/$ARCHIVE_NAME"

echo "Downloading $ARCHIVE_NAME..."
echo "From $URL"

if command -v kiss-fetch.sh > /dev/null 2>&1; then
  kiss-fetch.sh "$URL" -o "$TMP_FILE"
else
  echo "Warning: Kiss-fetch.sh missing"
  curl -L -Ss --fail "$URL" -o "$TMP_FILE"
fi

# Validate checksum
DOWNLOADED_SHA256=$(sha256sum "$TMP_FILE" | cut -d ' ' -f1)
if [ "$DOWNLOADED_SHA256" != "$EXPECTED_SHA256" ]; then
  echo "SHA256 mismatch!"
  echo "Expected: $EXPECTED_SHA256"
  echo "Got:      $DOWNLOADED_SHA256"
  rm -f "$TMP_FILE"
  exit 1
fi

# Extract and install
tar -xzf "$TMP_FILE"
cp "sccache-$SCCACHE_RELEASE-$PLATFORM-$TARGET_SUFFIX/sccache" "$TARGET_PATH"
chmod +x "$TARGET_PATH"

# Cleanup
rm -Rf "sccache-$SCCACHE_RELEASE-$PLATFORM-$TARGET_SUFFIX" "$TMP_FILE"

echo "sccache installed successfully to $TARGET_PATH"
