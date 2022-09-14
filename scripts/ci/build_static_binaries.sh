#!/bin/sh
set -eu

echo "Create destination directory"
mkdir -pv tezos-binaries

echo "Build and install static binaries"
./scripts/build_and_install_static_binaries.sh "tezos-binaries/${ARCH}"

echo "Strip debug symbols and compress binaries (parallelized)"
# shellcheck disable=SC2046,SC2038
find "tezos-binaries/$ARCH" -maxdepth 1 -type f ! -name "*.*" | xargs -n1 -P$(nproc) -i sh -c 'strip --strip-debug {}; upx -6q {};'