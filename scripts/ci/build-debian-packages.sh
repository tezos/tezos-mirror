#!/bin/sh
set -e

. scripts/version.sh
. scripts/releases/octez-release.sh

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# fetch tags for releases
git fetch -q --tags

# link the dependencies files in the build directory.
ln -s /root/tezos/_opam .

# Build octez debian packages
scripts/packaging/build-deb-local.sh "$1"

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -va scripts/packaging/*.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
