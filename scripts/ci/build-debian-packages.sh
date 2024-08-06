#!/bin/sh
set -e

. scripts/version.sh
. scripts/ci/octez-release.sh

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cd /root/tezos/

export DEBEMAIL="contact@nomadic-labs.com"

if [ -n "${gitlab_release_no_v:-}" ]; then
  DEBVERSION=$gitlab_release_no_v
  DEBCHANGELOG="New Release $gitlab_release_no_v / $CI_COMMIT_SHORT_SHA"
elif [ -n "$CI_COMMIT_TAG" ]; then
  DEBVERSION=$(date +'%Y%m%d%H%M')+$CI_COMMIT_TAG
  DEBCHANGELOG="Packages for tag $CI_COMMIT_TAG"
else
  DEBVERSION=$(date +'%Y%m%d%H%M')+$CI_COMMIT_SHORT_SHA
  DEBCHANGELOG="Test package commit $CI_COMMIT_REF_NAME"
fi

# Set a version for the debian package we are building.
debchange --changelog scripts/packaging/octez/debian/changelog \
  --newversion "$DEBVERSION" "$DEBCHANGELOG"

# Build octez debian packages
scripts/packaging/build-deb-local.sh "$1"

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -a scripts/packaging/*.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
