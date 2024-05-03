#!/bin/sh

# Build tezos as usual
eval "$(opam env)"
make all

# Prepare the packaging by copying all the freshly compiled binaries
mkdir -p scripts/packaging/octez/binaries
mkdir -p scripts/packaging/octez/zcash-params
cp octez-* scripts/packaging/octez/binaries/

# Copy the zcash parametes to be packaged
cp -a _opam/share/zcash-params scripts/packaging/octez/

# Build the debian packages
cd scripts/packaging/octez/ || exit 1
DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -b --no-sign -sa

echo "All packages are available in ./scripts/packaging"
