#!/bin/sh

# Install depedendencies for the apt_repo job

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y apt-utils debsigs gnupg curl

# Install google-cloud-cli so we can upload packages to the Google Cloud Storage bucket.
gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg \
  scripts/packaging/apt-key-gcloud.gpg

echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

apt-get update
apt-get --no-install-recommends -y install google-cloud-cli
