#!/bin/sh

distribution=$1
release=$2
rc=$3

# this is the default bucket using for testing
bucket="tezos-linux-repo"
if [ "${CI_COMMIT_REF_PROTECTED:-false}" = true ]; then
  # this is the release bucket used only on
  # protected release branches
  bucket="tezos-linux-protected-repo"
fi

if [ "$rc" = "rc" ]; then
  distribution="RC/$distribution"
fi

set -e
set -x

# [install prerequisites]
apt-get update
apt-get install -y sudo gpg curl
# [add repository]
REPO="deb https://$bucket.storage.googleapis.com/$distribution $release main"
sudo curl "https://$bucket.storage.googleapis.com/$distribution/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
sudo apt-get update

# [install tezos]
sudo apt-get install -y octez-client
sudo apt-get install -y octez-node
sudo apt-get install -y octez-baker

# [test executables]
octez-client --version
octez-node --version
octez-baker-Proxford --version
octez-accuser-Proxford --version
