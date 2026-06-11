#!/bin/sh

# Install the static binary of datadog-ci $DATADOG_RELEASE.
#
# Detects glibc vs musl (Alpine) and downloads the appropriate binary
# from the GitHub release page.
#
# Since v5.18.0, Datadog ships musl/Alpine-native binaries
# (datadog-ci_alpine-{x64,arm64}), so npm + Node.js are no longer
# needed in Alpine-based CI images.

# NOTE: no `pipefail`: Debian's /bin/sh (dash) rejects it. The only pipe
# is `sha256sum | cut` below; if sha256sum fails the result is empty,
# which won't match EXPECTED_SHA256 and exits 1 anyway.
set -eu

# When bumping DATADOG_RELEASE, also refresh the four SHA-256 checksums
# below AND the external [datadog/ci] image (tag + Docker Hub digest) pinned
# in ci/lib_tezos_ci/tezos_ci.ml (let datadog_ci). The two must stay in sync.
DATADOG_RELEASE="v5.18.0"

# Detect architecture
ARCH=$(uname -m)
case "$ARCH" in
x86_64) PLATFORM="x64" ;;
aarch64) PLATFORM="arm64" ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

# Detect libc: musl (Alpine) vs glibc (Debian/Ubuntu/Fedora).
# Detect musl by its loader file, not `ldd --version` (no --version on Alpine,
# exits non-zero, and varies across glibc distros such as Debian/Ubuntu/Fedora).
if ls /lib/ld-musl-*.so.* > /dev/null 2>&1; then
  VARIANT="alpine"
else
  VARIANT="linux"
fi

# SHA-256 checksums for v5.18.0 (from checksums.txt + manual verification)
case "${VARIANT}-${PLATFORM}" in
alpine-x64)
  EXPECTED_SHA256="29876c9a86769c4313265ca8e5a45abe8ac44a56a5924b781a3fe2192cb4ddcf"
  ;;
alpine-arm64)
  EXPECTED_SHA256="8ed33350e00e07f9ce243693a91ed4313526576e601cd7941bd63923e8f52ebb"
  ;;
linux-x64)
  EXPECTED_SHA256="b1ea0c7b288187fc429222de1a001b52b579e5825bea0b323dd55f8f181eea3b"
  ;;
linux-arm64)
  EXPECTED_SHA256="401e01ac6912f04369172d8d5c7a76e33a789ceaabe6ad95c06ab911843d2f77"
  ;;
*)
  echo "Unsupported variant/platform: ${VARIANT}-${PLATFORM}" >&2
  exit 1
  ;;
esac

# Define file path
TARGET_PATH="/usr/local/bin/datadog-ci"
TMP_FILE="$(mktemp)"

# Download datadog-ci
URL="https://github.com/DataDog/datadog-ci/releases/download/$DATADOG_RELEASE/datadog-ci_${VARIANT}-${PLATFORM}"
echo "Downloading datadog-ci ($VARIANT/$PLATFORM) from $URL..."

if command -v kiss-fetch.sh > /dev/null 2>&1; then
  kiss-fetch.sh -o "$TMP_FILE" "$URL"
else
  echo "Warning: Kiss-fetch.sh missing"
  curl -L -Ss --fail "$URL" -o "$TMP_FILE"
fi

DOWNLOADED_SHA256=$(sha256sum "$TMP_FILE" | cut -d ' ' -f1)
if [ "$DOWNLOADED_SHA256" != "$EXPECTED_SHA256" ]; then
  echo "SHA256 mismatch!"
  echo "Expected: $EXPECTED_SHA256"
  echo "Got:      $DOWNLOADED_SHA256"
  rm -f "$TMP_FILE"
  exit 1
fi

mv "$TMP_FILE" "$TARGET_PATH"
# mktemp creates files with restrictive perms (0600 under tightened umasks);
# install the binary 0755 so non-root users (e.g. tezos in the CI images)
# can read and execute it.
chmod 755 "$TARGET_PATH"

echo "datadog-ci installed successfully to $TARGET_PATH"

/usr/local/bin/datadog-ci --version
