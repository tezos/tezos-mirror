#!/bin/sh
set -e

deps_opam_repository_tag=$(cat /root/tezos/opam_repository_tag)
. scripts/version.sh

if [ "$deps_opam_repository_tag" != "$opam_repository_tag" ]; then
  echo "Dependency tag: $deps_opam_repository_tag"
  echo "Actual tag: $opam_repository_tag"
  echo "The dependency image is outdated. Please rebuild before lunching this job"
  exit 1
fi

BUILDDIR=$(pwd)

# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cd /root/tezos/

# Build tezos as usual
eval "$(opam env)"
make all

# Prepare the packaging by copying all the freshly compiled binaries
mkdir -p scripts/packaging/octez/binaries
mkdir -p scripts/packaging/octez/zcash-params
cp octez-* scripts/packaging/octez/binaries/

# Copy the zcash parametes to be packaged
cp -a _opam/share/zcash-params scripts/packaging/octez/

# Generate the octez-node manual to be included in the package
./octez-node --help=groff > scripts/packaging/octez/manpages/octez-node.1

# Build the debian packages
cd scripts/packaging/octez/
DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -b --no-sign -sa

# Move the debian package to be packed as artifacts
mkdir -p "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
mv ../*.deb "$BUILDDIR/packages/$DISTRIBUTION/$RELEASE"
