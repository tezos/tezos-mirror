#!/bin/sh
#

set -ue

# This script assumes that homebrew is correctly installed
# using the script ./scripts/packaging/homebrew_install.sh
# and creates a formula scripts/packaging/Formula/octez.rb
# that is ready to be installed with brew.

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

. scripts/ci/octez-packages-version.sh
case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  TARGETDIR="homebrew/RC/Formula"
  ;;
Release | TestRelease)
  TARGETDIR="homebrew/Formula"
  ;;
Master)
  TARGETDIR="homebrew/master/Formula"
  ;;
SoftRelease)
  TARGETDIR="homebrew/${CI_COMMIT_TAG}/Formula"
  ;;
TestBranch | TestProtectedBranch)
  TARGETDIR="homebrew/$CI_COMMIT_REF_NAME/Formula"
  ;;
*)
  echo "Cannot create a repository for this branch"
  exit 1
  ;;
esac

# brew cannot install a formula directly from a URL, so fetch it first.
case "$RELEASETYPE" in
TestBranch)
  # Formulae of unprotected refs live in a bucket that require GCP
  # authentication.
  echo "installing formula from gs://$BUCKET/$TARGETDIR/octez.rb"
  . scripts/ci/gcp_auth.sh
  gsutil -q cp "gs://$BUCKET/$TARGETDIR/octez.rb" .
  ;;
*)
  # Protected refs publish to a bucket used over https.
  echo "installing formula from https://$BUCKET.storage.googleapis.com/$TARGETDIR/octez.rb"
  curl -q "https://$BUCKET.storage.googleapis.com/$TARGETDIR/octez.rb" -O
  ;;
esac
# Create pre-compiled bottle from local octez formula
# with verbose output and developer checks enabled
export HOMEBREW_DEVELOPER=1
# allow to pass env vars while building the bottle
export HOMEBREW_NO_ENV_FILTERING=1
# do not upgrade homebrew automatically
export HOMEBREW_NO_AUTO_UPDATE=1

brew install -v --formula --build-bottle ./octez.rb

octez-node --version
