#!/bin/sh
set -eu

cd /

# Install dependencies
apk add --no-cache bash curl python3 py3-crcmod libc6-compat

# Download gcloud SDK archive
curl -O "https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz"

# Extract and install
tar -xf "google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz"
rm "google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz"

/google-cloud-sdk/install.sh --quiet
export PATH="$PATH:/google-cloud-sdk/bin"
gcloud components install --quiet core gsutil kubectl docker-credential-gcr
