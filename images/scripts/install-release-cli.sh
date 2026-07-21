#!/bin/sh
# Install GitLab release-cli (v0.11.0)
# --------------------------------------------------------------

set -eu

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
VERSION="v0.11.0"
BASE_URL="https://gitlab.com/gitlab-org/release-cli/-/releases/${VERSION}/downloads/bin"

# Determine architecture (matches GitLab naming)
case "$(uname -m)" in
x86_64) ARCH="amd64" ;;
aarch64) ARCH="arm64" ;;
*)
  echo "Unsupported architecture: $(uname -m)" >&2
  exit 1
  ;;
esac

DOWNLOAD_URL="${BASE_URL}/release-cli-linux-${ARCH}"
TARGET="/usr/local/bin/release-cli"

# ----------------------------------------------------------------------
# Installation
# ----------------------------------------------------------------------
if command -v kiss-fetch.sh > /dev/null 2>&1; then
  echo "Kiss-fetch.sh available. Trying to use kisscache."
  kiss-fetch.sh -o "$TARGET" "$DOWNLOAD_URL"
else
  echo "Warning: Kiss-fetch.sh missing"
  curl -L -Ss --fail "$DOWNLOAD_URL" -o "$TARGET"
fi
chmod 755 "$TARGET"

# Verify the binary is actually runnable before declaring success, so a
# truncated or corrupt download fails the build instead of the image.
"$TARGET" --version

printf '\n release-cli %s installed for %s\n' "$VERSION" "$ARCH"
