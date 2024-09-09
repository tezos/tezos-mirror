#!/bin/sh
set -e

BUILDDIR=$(pwd)
export BLST_PORTABLE=true

# fetch tags for releases
git fetch --tags -q

# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cp -a ./.git /root/tezos/
cd /root/tezos/

# shellcheck disable=SC1091
. "$HOME/.cargo/env"
eval "$(opam env)"
eval "$(dpkg-architecture)"

# Build octez debian packages
make "dpkg-$1"

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -a -- *.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
