#!/bin/sh
set -e

. scripts/version.sh
. scripts/ci/octez-release.sh

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# fetch tags for releases
git fetch -q --tags

# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cp -a ./.git /root/tezos/
cd /root/tezos/

eval "$(opam env)"
eval "$(dpkg-architecture)"

export DEBEMAIL="contact@nomadic-labs.com"

if [ -n "${gitlab_release_no_v:-}" ]; then
  DEBVERSION=$gitlab_release_no_v
  DEBCHANGELOG="New Release $gitlab_release_no_v / $CI_COMMIT_SHORT_SHA"
  EXPECTED_VERSION="Octez $gitlab_release_no_v"
elif [ -n "$CI_COMMIT_TAG" ]; then
  DEBVERSION=$(date +'%Y%m%d%H%M')+$CI_COMMIT_TAG
  DEBCHANGELOG="Packages for tag $CI_COMMIT_TAG"
  EXPECTED_VERSION="Octez 0.0+dev"
else
  DEBVERSION=$(date +'%Y%m%d%H%M')+$CI_COMMIT_SHORT_SHA
  DEBCHANGELOG="Test package commit $CI_COMMIT_REF_NAME"
  EXPECTED_VERSION="Octez 0.0+dev"
fi

# Set a version for the debian package we are building.
debchange --changelog scripts/packaging/octez/debian/changelog \
  --newversion "$DEBVERSION" "$DEBCHANGELOG"

# Build octez debian packages
scripts/packaging/build-deb-local.sh "$1"

OCTEZ_VERSION=$(dune exec src/lib_version/exe/octez_print_version.exe)
echo "Version used for the package: $DEBVERSION"
if [ "$OCTEZ_VERSION" != "$EXPECTED_VERSION" ]; then
  echo "Version of the octez: $OCTEZ_VERSION"
  echo "Expected version: $EXPECTED_VERSION"
  exit 1
fi

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -a scripts/packaging/*.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
