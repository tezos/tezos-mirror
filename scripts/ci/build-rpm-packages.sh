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

scripts/packaging/build-rpm-local.sh "$1"

# Move the rpm package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -va "$HOME"/rpmbuild/RPMS/* \
  "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
