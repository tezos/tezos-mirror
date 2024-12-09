#!/bin/sh
#

set -eu

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/next/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# Update and install the config-mananger plugin
dnf -y update
dnf -y install 'dnf-command(config-manager)'

# Add the repository
dnf -y config-manager --add-repo "$REPO/$DISTRO/dists/$RELEASE"
dnf -y update

# Install public key
rpm --import "$REPO/$DISTRO/octez.asc"

dnf -y install octez-node
rpm -v --info -q octez-node
rpm -v --verify octez-node
