#!/bin/sh

set -eu

# Build tezos as usual
# shellcheck disable=SC1091
. "$HOME/.cargo/env"
eval "$(opam env)"
make all

# Prepare the packaging by copying all the freshly compiled binaries
mkdir -p scripts/packaging/octez/binaries
mkdir -p scripts/packaging/octez/zcash-params
rm -Rf scripts/packaging/octez/binaries/*

EXECUTABLES=$(cat script-inputs/*-executables)
for ex in $EXECUTABLES; do
  cp "$ex" scripts/packaging/octez/binaries/
done

# Copy the zcash parametes to be packaged
cp -a _opam/share/zcash-params scripts/packaging/octez/

# Build the debian packages
rm -f scripts/packaging/*.deb
cd scripts/packaging/octez/
DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -tc -b --no-sign -sa
cd -

echo "All packages are available in ./scripts/packaging"

# Run lintian only if building packages locally.
# On the CI we have a specific job for it
if [ -z "${CI:-}" ]; then
  lintian scripts/packaging/octez-*.deb --tag-display-limit 0 --verbose
fi
