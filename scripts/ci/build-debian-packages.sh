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

# Build octez debian packages
scripts/packaging/build-deb-local.sh "$1"

#OCTEZ_VERSION=$(dune exec src/lib_version/exe/octez_print_version.exe)
#echo "Version used for the package/octez: $DEBVERSION / $OCTEZ_VERSION"
#if [ -n "${EXPECTED_VERSION:-}" ] && [ "$OCTEZ_VERSION" != "$EXPECTED_VERSION" ]; then
#echo "Executables version does not match the expected version of the packages:"
#echo "Executables version: $OCTEZ_VERSION"
#echo "Expected version: $EXPECTED_VERSION"
#exit 1
#fi

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
cp -va scripts/packaging/*.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
