#!/bin/sh

distribution=$1
release=$2

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

case "$RELEASETYPE" in
Master | Release | ReleaseCandidate)
  # Production publications, served as packages.nomadic-labs.com and installed
  # exactly as howtoget.rst documents.
  apt_get update
  apt_get install -y sudo

  # [add repository]
  sudo apt-get install -y gpg curl
  curl -s "https://packages.nomadic-labs.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  echo "deb [signed-by=/etc/apt/keyrings/octez.gpg] https://packages.nomadic-labs.com/$distribution $release main" |
    sudo tee /etc/apt/sources.list.d/octez.list
  sudo apt-get update
  sudo apt-get install -y octez-archive-keyring
  sudo sed -i 's|signed-by=/etc/apt/keyrings/octez.gpg|signed-by=/usr/share/keyrings/octez-archive-keyring.gpg|' \
    /etc/apt/sources.list.d/octez.list
  sudo apt-get update
  # [end add repository]
  ;;
*)
  # Test publications live in the bucket of the ref: see
  # scripts/ci/packages_bucket.inc.sh.
  . scripts/ci/packages_bucket.inc.sh
  bucket="$PACKAGES_BUCKET"
  apt_get update
  apt_get install -y sudo gpg curl
  case "$RELEASETYPE" in
  TestBranch)
    # Assets for unprotected refs require authentication
    . scripts/ci/gcp_auth.sh
    repo_dir=/tmp/octez-repo
    gcs_mirror "gs://$bucket/$distribution" "$repo_dir"
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg "$repo_dir/octez.asc"
    REPO="deb [signed-by=/etc/apt/keyrings/octez.gpg] file:$repo_dir $release main"
    ;;
  *)
    # Protected refs assets require no authentication
    curl -s "https://$bucket.storage.googleapis.com/$distribution/octez.asc" |
      sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
    REPO="deb [signed-by=/etc/apt/keyrings/octez.gpg] https://$bucket.storage.googleapis.com/$distribution $release main"
    ;;
  esac
  echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
  apt_get update
  ;;
esac
