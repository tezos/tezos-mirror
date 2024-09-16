#!/bin/sh
#

set -ue

# This script assume that homebrew is correctly installed
# using the script ./scripts/packaging/homebrew_install.sh
# and creates a formula scripts/packaging/Formula/octez.rb
# that is ready to be installed with brew

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

# set version

. scripts/version.sh
. scripts/ci/octez-release.sh

# prepare target dir

if [ -n "${gitlab_release_no_v:-}" ]; then
  # if it's a release tag, then it can be a RC release or a final release
  if [ -n "${gitlab_release_rc_version}" ]; then
    # Release candidate
    TARGETDIR="homebrew/RC/Formula"
  else
    # Release
    TARGETDIR="homebrew/Formula"
  fi
else
  if [ "$CI_COMMIT_REF_PROTECTED" = "false" ]; then
    if [ "$CI_COMMIT_REF_NAME" = "RC" ]; then
      echo "Cannot create a repository for a branch named 'RC'"
      exit 1
    else
      # Branch is not protected, this is for testing ordinary MRs
      TARGETDIR="homebrew/$CI_COMMIT_REF_NAME/Formula"
    fi
  else
    # For protected branches that are not release, we allow
    # a repository only for master.
    if [ "$CI_COMMIT_REF_NAME" = "master" ]; then
      TARGETDIR="homebrew/master/Formula"
    else
      if [ -n "${CI_COMMIT_TAG}" ]; then
        TARGETDIR="homebrew/${CI_COMMIT_TAG}/Formula"
      else
        echo "Cannot create a repository for a protected branch that is not associated to a tag or master"
        exit 1
      fi
    fi
  fi
fi

echo "installing formula from https://$BUCKET.storage.googleapis.com/$TARGETDIR/octez.rb"

# get around the fact that we cannot install a formula directly from https
curl -q "https://$BUCKET.storage.googleapis.com/$TARGETDIR/octez.rb" -O
brew install -v ./octez.rb

octez-node --version

#brew audit --strict octez
