#!/bin/sh

set -eu

# PACKAGES_BUCKET is not derived here. This script runs inside the systemd
# Docker container started by scripts/packaging/tests/systemd-docker-test.sh
# (in the CI through scripts/ci/systemd-packages-test.sh): that harness
# sources scripts/ci/packages_bucket.inc.sh on the host and passes
# PACKAGES_BUCKET into the container with docker exec -e.
REPO="https://storage.googleapis.com/${PACKAGES_BUCKET:?must be set, see scripts/ci/packages_bucket.inc.sh}/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# include apt_get function with retry
. scripts/packaging/tests/tests-common.inc.sh
set_octez_repo_urls

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

apt_get update
apt_get install -y sudo gpg curl apt-utils debconf-utils procps jq

sudo curl "$REPO_FETCH_URL/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg

# [add next repository]
repository="deb $REPO_URL $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-next.list
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
