#!/usr/bin/env bash

set -euo pipefail

# The following protocols are not needed for tests.
# By removing them, we cause them not to be compiled and linked and the CI runs faster.
# At least the following other jobs still compile with all protocols:
# - Docker images (for master and releases)
# - static binaries (only for releases)

usage() {
  echo "Usage: $0 [<protocol-trashbin>]"
  echo "Removes old protocols. If <protocol-trashbin> is given, they are put there"
  echo "and the effect of this script can be reversed by: "
  echo "'scripts/restore-old-protocols.sh <protocol-trashbin>'"
  exit 1
}

trash_bin=""
if [ -n "${1:-}" ] && [ "${1:-}" != "--help" ]; then
  trash_bin=$1
  mkdir -p "$trash_bin"
elif [ "${1:-}" = "--help" ]; then
  usage
fi

all_protocols=$(find src -maxdepth 1 -type d -regex 'src/proto.*' |
  cut -d '_' -f2- |
  sed 's/_/-/g')

to_be_removed=$(echo "$all_protocols" |
  grep -wvFf script-inputs/active_protocol_versions |
  grep -wvFf script-inputs/active_testing_protocol_versions |
  sed 's/-/_/g')

for proto in $to_be_removed; do
  if [ -z "$trash_bin" ]; then
    echo rm -rf src/proto_"${proto}"
    rm -rf src/proto_"${proto}"
  else
    echo mv src/proto_"${proto}" "$trash_bin"
    mv src/proto_"${proto}" "$trash_bin"
  fi
done
