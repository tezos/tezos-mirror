#!/bin/sh

# Shared setup for keyring test scripts.
# Source this file from test-keyring-user-journey.sh and test-keyring-ci-checks.sh.

REPO="https://storage.googleapis.com/${GCP_LINUX_PACKAGES_BUCKET:-tezos-linux-repo}/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2
KEYRING="/usr/share/keyrings/octez-archive-keyring.gpg"

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

export DEBIAN_FRONTEND=noninteractive

# Install dependencies, bootstrap the APT repository with a manual key,
# install the keyring package, and switch APT to use the managed keyring.
setup_keyring_test() {
  echo "=== Setup: install dependencies ==="
  # Use the apt_get retry wrapper (sourced above) rather than raw apt-get:
  # the public Debian/Ubuntu mirrors intermittently fail with "Unknown error
  # executing apt-key" / "Mirror sync in progress", which the wrapper absorbs
  # by retrying. A raw apt-get here makes the whole job flaky.
  apt_get update
  apt_get install -y sudo gpg gpg-agent curl apt-utils

  echo "=== Setup: bootstrap repository with manual key ==="
  # Clean up any stale state from a previous run in the same container
  sudo rm -f /etc/apt/keyrings/octez.gpg
  sudo rm -f /etc/apt/sources.list.d/octez.list
  sudo mkdir -p /etc/apt/keyrings
  sudo curl -fsSL "$REPO/$DISTRO/octez.asc" |
    sudo gpg --batch --yes --dearmor -o /etc/apt/keyrings/octez.gpg

  echo "deb [signed-by=/etc/apt/keyrings/octez.gpg] $REPO/$DISTRO $RELEASE main" |
    sudo tee /etc/apt/sources.list.d/octez.list
  apt_get update

  echo "=== Setup: install keyring and switch APT to managed keyring ==="
  apt_get install -y octez-archive-keyring
  sudo sed -i "s|signed-by=/etc/apt/keyrings/octez.gpg|signed-by=$KEYRING|" \
    /etc/apt/sources.list.d/octez.list
  apt_get update
}
