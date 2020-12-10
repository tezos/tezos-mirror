#! /usr/bin/env bash

set -euo pipefail

ARCH_PREFIX=$1

. scripts/release/binaries.sh

for binary in "${binaries[@]}"; do
    curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
         --upload-file "install_root/bin/$binary" \
         "$PACKAGE_REGISTRY_URL/${ARCH_PREFIX}$binary"
done

# Create .tag.gz archive with all binaries and upload it
cd install_root/bin
tar -czf ../../tezos-binaries.tar.gz .
cd ../..
curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
     --upload-file tezos-binaries.tar.gz \
     "$PACKAGE_REGISTRY_URL/${ARCH_PREFIX}tezos-binaries.tar.gz"
