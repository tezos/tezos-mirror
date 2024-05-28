#!/bin/sh

set -e

usage="Usage:
$ ./scripts/link_protocol.sh src/proto_<new_version>_<new_hash>

This updates manifest/main.ml to add the new protocol.
Then, it runs make -C manifest to regenerate the relevant files.
"

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

new_version="$(basename "$1" | awk -F'_' '{print $2}')"
new_hash="$(basename "$1" | awk -F'_' '{print $3}')"

if [ -z "${new_version}" ]; then
  echo "$usage"
  exit 1
fi

if [ -z "${new_hash}" ]; then
  echo "$usage"
  exit 1
fi

sed "/let alpha = active Name.alpha/i \  let _${new_version}_${new_hash} = active (Name.v \"${new_hash}\" ${new_version})\n" -i manifest/main.ml

# Generate everything from the manifest.
echo "Updating manifest: generate dune and opam files..."
make -C manifest
