#! /usr/bin/env bash

set -euo pipefail

. scripts/release/binaries.sh

for binary in "${binaries[@]}"; do
    curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
         --upload-file "install_root/bin/$binary" \
         "$PACKAGE_REGISTRY_URL/$binary"
done
