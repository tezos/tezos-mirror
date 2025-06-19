#!/bin/sh
#

set -e

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# fetch tags for releases
git fetch -q --tags

# link the dependencies files in the build directory.
ln -s /root/tezos/_opam .

scripts/packaging/build-rpm-local.sh "$1"

# Move the rpm package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -va "$HOME"/rpmbuild/RPMS/* \
  "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
