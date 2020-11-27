#! /usr/bin/env bash

set -euo pipefail

. scripts/release/binaries.sh

for binary in "${binaries[@]}"; do
    curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
         --upload-file "install_root/bin/$binary" \
         "$PACKAGE_REGISTRY_URL/$binary"
done

# Create .tag.gz archive with all binaries and upload it
cd install_root/bin
tar -czf ../../tezos-binaries.tar.gz .
cd ../..
curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
     --upload-file tezos-binaries.tar.gz \
     "$PACKAGE_REGISTRY_URL/tezos-binaries.tar.gz"
