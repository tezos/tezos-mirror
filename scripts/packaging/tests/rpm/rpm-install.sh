#!/bin/sh
#

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

set -eu

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# wait for systemd to be ready
count=0
while [ "$(systemctl is-system-running)" = "offline" ]; do
  count=$((count + 1))
  if [ $count -ge 10 ]; then
    echo "System is not running after 10 iterations."
    exit 1
  fi
  sleep 1
done

# Update and install the config-manager plugin
dnf -y update
dnf -y install dnf-plugins-core

# Add the repository
dnf -y config-manager --add-repo "$REPO/$DISTRO/dists/$RELEASE"
if [ "$DISTRO" = "rockylinux" ]; then
  dnf -y config-manager --set-enabled devel
fi
dnf -y update

# Install public key
rpm --import "$REPO/$DISTRO/octez.asc"

dnf -y install sudo procps util-linux

dnf -y install \
  octez-client \
  octez-node \
  octez-dal-node \
  octez-baker \
  octez-smart-rollup-node

octez-node --version
octez-client --version
octez-dal-node --version
octez-baker --version
octez-smart-rollup-node --version

dnf -y remove octez-node \
  octez-client \
  octez-baker \
  octez-dal-node \
  octez-smart-rollup-node
