#!/bin/sh

# Install depedendencies to use gsutils

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y gpg curl

# Install google-cloud-cli so we can upload packages to the Google Cloud Storage bucket.
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg |
  gpg --dearmor -o /usr/share/keyrings/cloud.google.gpg

echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

apt-get update
apt-get --no-install-recommends -y install google-cloud-cli
