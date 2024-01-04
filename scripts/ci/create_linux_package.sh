#!/usr/bin/env bash

set -eu

# shellcheck disable=SC2155
export HOME=$(pwd)

# Latest v* tag will be used as the version.
git fetch --tags

git clone "$TEZOS_PACKAGING_REPO" tezos-packaging
cp -R "$TEZOS_BINARIES" tezos-packaging/binaries

cd tezos-packaging
git checkout "$TEZOS_PACKAGING_VERSION"

cat << DOC > meta.json
{
    "release": "1",
    "maintainer": "Tezos devteam <contact@tezos.com>"
}
DOC

# NOTE: The package generator script relies on the binaries having the `tezos-` prefix instead of
# `octez-*` which is currently in place.
# This is a temporary fix until the upstream version supports the renamed `octez` binaries.
cd binaries
# shellcheck disable=SC2001
for f in octez*; do mv "$f" "$(echo "$f" | sed s/octez/tezos/g)"; done
cd ../

export DEB_BUILD_OPTIONS=nostrip

if [ "$PACKAGE_FORMAT" = "deb" ]; then
  mkdir -p ../dist/debian
  python3 -m docker.package.package_generator --os ubuntu --type binary --binaries-dir ./binaries --build-sapling-package
  mv ./*.{deb,changes,buildinfo} ../dist/debian
else
  mkdir -p ../dist/fedora
  python3 -m docker.package.package_generator --os fedora --type binary --binaries-dir ./binaries --build-sapling-package
  mv out/*.rpm ../dist/fedora
fi
