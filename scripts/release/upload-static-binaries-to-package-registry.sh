#!/bin/sh

set -eu

ARCH_PREFIX=$1
binaries=

. scripts/release/binaries.sh || exit 1

for binary in $binaries; do
    curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
         --upload-file "tezos-binaries/${ARCH_PREFIX}/$binary" \
         "$PACKAGE_REGISTRY_URL/${ARCH_PREFIX}-$binary"
    echo "Upload binary to $PACKAGE_REGISTRY_URL/${ARCH_PREFIX}-$binary"
done

# Create .tag.gz archive with all binaries and upload it
mkdir -p "tezos-binaries/tezos-${ARCH_PREFIX}"
cp -a tezos-binaries/"${ARCH_PREFIX}"/* "tezos-binaries/tezos-${ARCH_PREFIX}/"

cd "tezos-binaries/" && \
  tar -czf "tezos-${ARCH_PREFIX}.tar.gz" "tezos-${ARCH_PREFIX}/"

rm -Rf "tezos-binaries/tezos-${ARCH_PREFIX}"

curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
     --upload-file "tezos-${ARCH_PREFIX}.tar.gz" \
     "$PACKAGE_REGISTRY_URL/tezos-${ARCH_PREFIX}.tar.gz"

echo "Uploaded binary bundle to $PACKAGE_REGISTRY_URL/tezos-${ARCH_PREFIX}.tar.gz"
