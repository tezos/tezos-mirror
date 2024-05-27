#!/bin/sh
set -e

. scripts/version.sh

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cd /root/tezos/

# Build octez debian packages
scripts/packaging/build-deb-local.sh

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -a scripts/packaging/*.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
