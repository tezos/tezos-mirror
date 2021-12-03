#!/bin/sh

set -eu

ARCH_PREFIX=$1
binaries=

. scripts/release/binaries.sh || exit 1

for binary in $binaries; do
    curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
         --upload-file "tezos-binaries/$binary" \
         "$PACKAGE_REGISTRY_URL/${ARCH_PREFIX}$binary"
    echo "Upload binary to $PACKAGE_REGISTRY_URL/${ARCH_PREFIX}$binary"
done

# Create .tag.gz archive with all binaries and upload it
tar -czf tezos-binaries.tar.gz tezos-binaries
curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
     --upload-file tezos-binaries.tar.gz \
     "$PACKAGE_REGISTRY_URL/${ARCH_PREFIX}tezos-binaries.tar.gz"

echo "Upload binary bundle to $PACKAGE_REGISTRY_URL/${ARCH_PREFIX}tezos-binaries.tar.gz"

