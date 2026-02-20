#!/bin/sh

# The following is needed to allow using "sudo apt-get" below, even though apt-get is
# defined as an alias in library scripts/packaging/tests/tests-common.inc.sh
# shellcheck disable=SC2033

distribution=$1
release=$2

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
bucket="$GCP_LINUX_PACKAGES_BUCKET"

# This logic must be kept in sync with the script in
# ./scripts/ci/create_debian_repo.sh

# The prefix used for these packages in the repository. E.g. 'old'
if [ -n "$PREFIX" ]; then
  PREFIX=${PREFIX}/
else
  PREFIX=
fi

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

if [ -n "$CI" ]; then
  . scripts/ci/octez-packages-version.sh
fi

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  distribution="${PREFIX}RC/${distribution}"
  ;;
Release | TestRelease)
  # use $distribution as it is
  : nop
  ;;
Master)
  distribution="${PREFIX}master/${distribution}"
  ;;
SoftRelease)
  distribution="${PREFIX}${CI_COMMIT_TAG}/${distribution}"
  ;;
TestBranch)
  distribution="${PREFIX}${CI_COMMIT_REF_NAME}/${distribution}"
  ;;
*)
  echo "Cannot test packages on this branch"
  exit 1
  ;;
esac

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

set -e
set -x

if [ "$RELEASETYPE" = "Master" ]; then
  apt-get update
  apt-get install -y sudo

  # [add repository]
  sudo apt-get install -y gpg curl
  curl -s "https://packages.nomadic-labs.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  echo "deb [signed-by=/etc/apt/keyrings/octez.gpg] https://packages.nomadic-labs.com/$distribution $release main" |
    sudo tee /etc/apt/sources.list.d/octez.list
  sudo apt-get update
  # [end add repository]
else
  apt-get update
  apt-get install -y sudo gpg curl
  curl -s "https://$bucket.storage.googleapis.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  REPO="deb [signed-by=/etc/apt/keyrings/octez.gpg] https://$bucket.storage.googleapis.com/$distribution $release main"
  echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
  apt-get update
fi
