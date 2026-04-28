#!/bin/sh

# Install dependencies to use gsutils

ARCH=$(uname -m)
case "$ARCH" in
x86_64) GCLOUD_ARCH="x86_64" ;;
aarch64) GCLOUD_ARCH="arm" ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

BASE_URL="https://dl.google.com/dl/cloudsdk/channels/rapid/downloads"
TARBALL="google-cloud-cli-linux-${GCLOUD_ARCH}.tar.gz"

kiss-fetch.sh -fsSL -o "/tmp/${TARBALL}" "${BASE_URL}/${TARBALL}"

EXPECTED=$(kiss-fetch.sh -fsSL "https://dl.google.com/dl/cloudsdk/release/sha256.txt" |
  grep "downloads/${TARBALL}" | awk '{print $1}')
if [ -z "$EXPECTED" ]; then
  echo "Failed to fetch checksum for ${TARBALL}" >&2
  exit 1
fi

ACTUAL=$(sha256sum "/tmp/${TARBALL}" | awk '{print $1}')
if [ "$EXPECTED" != "$ACTUAL" ]; then
  echo "Checksum verification failed for ${TARBALL}" >&2
  echo "Expected: ${EXPECTED}" >&2
  echo "Actual:   ${ACTUAL}" >&2
  exit 1
fi

tar -xzC /usr/local/ -f "/tmp/${TARBALL}"
rm "/tmp/${TARBALL}"

/usr/local/google-cloud-sdk/install.sh --quiet --path-update false
ln -sf /usr/local/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud
ln -sf /usr/local/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil
