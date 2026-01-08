#!/bin/sh

# Install the static opam binary from the official release with SHA256 verification

set -eu

OPAM_VERSION="${OPAM_VERSION:?"Please call this script with OPAM_VERSION"}"

# Architecture detection
ARCH=$(uname -m)
case "$ARCH" in
i386 | i686 | x86_64)
  PLATFORM="i686"
  BINARY_URL="https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-i686-linux"
  EXPECTED_SHA256="fa64561eb1b3128a825dd4b8a4a6143d77485d2ab373ca393e948d9a62668afa"
  ;;
aarch64)
  PLATFORM="arm64"
  BINARY_URL="https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-arm64-linux"
  EXPECTED_SHA256="c363d070b1771b6830c6806eb9fd5ff1a54d0fe97bf389cd2dff20b6d203b2ad"
  ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

TARGET_PATH="/usr/bin/opam"
TMP_FILE="$(mktemp)"

echo "Downloading opam ${OPAM_VERSION} for $PLATFORM..."
curl -L -o "$TMP_FILE" "$BINARY_URL"

DOWNLOADED_SHA256=$(sha256sum "$TMP_FILE" | cut -d ' ' -f1)
if [ "$DOWNLOADED_SHA256" != "$EXPECTED_SHA256" ]; then
  echo "SHA256 mismatch!"
  echo "Expected: $EXPECTED_SHA256"
  echo "Got:      $DOWNLOADED_SHA256"
  rm -f "$TMP_FILE"
  exit 1
fi

cp "$TMP_FILE" "$TARGET_PATH"
chmod +x "$TARGET_PATH"
rm -f "$TMP_FILE"

echo "opam ${OPAM_VERSION} installed successfully to $TARGET_PATH"
