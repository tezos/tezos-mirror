#!/bin/sh

set -eu

REPO="https://storage.googleapis.com/${GCP_LINUX_PACKAGES_BUCKET:-tezos-linux-repo}/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# include apt_get function with retry
. scripts/packaging/tests/tests-common.inc.sh

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

apt_get update
apt_get install -y sudo gpg curl apt-utils debconf-utils procps jq

# Bootstrap: download the signing key to set up the repository
sudo mkdir -p /etc/apt/keyrings
sudo curl -fsSL "$REPO/$DISTRO/octez.asc" |
  sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg

repository="deb [signed-by=/etc/apt/keyrings/octez.gpg] $REPO/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez.list
apt_get update

# Install keyring package and switch APT to managed keyring
apt_get install -y octez-archive-keyring
sudo sed -i 's|signed-by=/etc/apt/keyrings/octez.gpg|signed-by=/usr/share/keyrings/octez-archive-keyring.gpg|' \
  /etc/apt/sources.list.d/octez.list
apt_get update

apt_get install -y \
  octez-client \
  octez-node \
  octez-dal-node \
  octez-baker \
  octez-smart-rollup-node

systemctl list-unit-files --type=service | grep "octez"

octez-node --version
octez-client --version
octez-dal-node --version
octez-baker --version
octez-smart-rollup-node --version

apt_get autopurge -y \
  octez-client \
  octez-node \
  octez-dal-node \
  octez-baker
