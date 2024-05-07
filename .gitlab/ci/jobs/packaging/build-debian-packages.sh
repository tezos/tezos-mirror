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
