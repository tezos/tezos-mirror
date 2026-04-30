#!/bin/sh

# Install dependencies to use gsutils

GCLOUD_VERSION="566.0.0"
GCLOUD_SHA256_X86_64="22513f71d7bf3af39ddf046d8b136ab82595859ecfcc6a4a9e7a37d70a183300"
GCLOUD_SHA256_ARM="9dd8ccf1f0243585c995b53bc2059c26e048841963804935c584b6e6f8d8f298"

ARCH=$(uname -m)
case "$ARCH" in
x86_64)
  GCLOUD_ARCH="x86_64"
  GCLOUD_SHA256="$GCLOUD_SHA256_X86_64"
  ;;
aarch64)
  GCLOUD_ARCH="arm"
  GCLOUD_SHA256="$GCLOUD_SHA256_ARM"
  ;;
*)
  echo "Unsupported architecture: $ARCH" >&2
  exit 1
  ;;
esac

BASE_URL="https://dl.google.com/dl/cloudsdk/channels/rapid/downloads"
TARBALL="google-cloud-cli-${GCLOUD_VERSION}-linux-${GCLOUD_ARCH}.tar.gz"

kiss-fetch.sh -fsSL -o "/tmp/${TARBALL}" "${BASE_URL}/${TARBALL}"

ACTUAL=$(sha256sum "/tmp/${TARBALL}" | awk '{print $1}')
if [ "$ACTUAL" != "$GCLOUD_SHA256" ]; then
  echo "Checksum verification failed for ${TARBALL}" >&2
  echo "Expected: ${GCLOUD_SHA256}" >&2
  echo "Actual:   ${ACTUAL}" >&2
  exit 1
fi

tar -xzC /usr/local/ -f "/tmp/${TARBALL}"
rm "/tmp/${TARBALL}"

/usr/local/google-cloud-sdk/install.sh --quiet --path-update false
ln -sf /usr/local/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud
ln -sf /usr/local/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil
