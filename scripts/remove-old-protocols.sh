#!/usr/bin/env bash

set -euo pipefail

# The following protocols are not needed for tests.
# By removing them, we cause them not to be compiled and linked and the CI runs faster.
# At least the following other jobs still compile with all protocols:
# - Docker images (for master and releases)
# - static binaries (only for releases)
# Note: src/proto_000_Ps9mPmXa is needed by Flextesa

all_protocols=$(find src -maxdepth 1 -type d -regex 'src/proto.*' | \
                cut -d '_' -f2- | \
                sed -r 's/_/-/g')

to_be_removed=$(echo "$all_protocols" | \
                grep -wvFf ./active_protocol_versions | \
                grep -wvFf ./active_testing_protocol_versions | \
                sed -r 's/-/_/g')

for proto in $to_be_removed; do
  echo rm -r src/proto_"${proto}"
  rm -r src/proto_"${proto}"
done
