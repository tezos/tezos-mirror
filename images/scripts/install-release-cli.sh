#!/bin/sh
# Install GitLab release-cli (v0.11.0)
# --------------------------------------------------------------

set -e

# ----------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------
VERSION="v0.11.0"
BASE_URL="https://gitlab.com/gitlab-org/release-cli/-/releases/${VERSION}/downloads/bin"

# Determine architecture (matches GitLab naming)
case "$(uname -m)" in
x86_64) ARCH="amd64" ;;
aarch64) ARCH="arm64" ;;
*) die "Unsupported architecture: $(uname -m)" ;;
esac

DOWNLOAD_URL="${BASE_URL}/release-cli-linux-${ARCH}"
TARGET="/usr/local/bin/release-cli"

# ----------------------------------------------------------------------
# Installation
# ----------------------------------------------------------------------
if command -v kiss-fetch.sh > /dev/null 2>&1; then
  kiss-fetch.sh "$DOWNLOAD_URL" -o "$TARGET"
else
  echo "Warning: Kiss-fetch.sh missing"
  curl -L -Ss --fail "$DOWNLOAD_URL" -o "$TARGET"
fi
chmod 755 "$TARGET"

printf '\n release-cli %s installed for %s (%s)\n' "$VERSION" "$DIST_ID" "$ARCH"
