#!/bin/sh
#

set -e

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# fetch tags for releases
git fetch -q --tags

# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cp -a ./.git /root/tezos/
cd /root/tezos/ || exit 1

. scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate | Release | TestRelease)
  _VERSION=$VERSION
  _CHANGELOG="New Release $VERSION / $CI_COMMIT_SHORT_SHA"
  ;;
Master)
  _VERSION=1:$(date +'%Y%m%d%H%M')+$CI_COMMIT_SHORT_SHA
  _CHANGELOG="Packages for master $CI_COMMIT_SHORT_SHA"
  ;;
SoftRelease)
  _VERSION=1:$(date +'%Y%m%d%H%M')+${CI_COMMIT_TAG:-}
  _CHANGELOG="Packages for tag ${CI_COMMIT_TAG:-}"
  ;;
TestBranch)
  _VERSION=1:$(date +'%Y%m%d%H%M')+$CI_COMMIT_SHORT_SHA
  _CHANGELOG="Test package commit ${CI_COMMIT_REF_NAME:-}"
  ;;
*)
  echo "Cannot create package for this branch"
  exit 1
  ;;
esac

scripts/packaging/build-rpm-local.sh "$1"

# Move the rpm package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -va "$HOME"/rpmbuild/RPMS/* \
  "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
