#!/bin/sh
#

set -eu

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# Update and install the config-mananger plugin
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

dnf -y install octez-node
rpm -v --info -q octez-node
rpm -v --verify octez-node
