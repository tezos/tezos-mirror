#!/bin/sh

set -e

usage="Usage:
$ ./scripts/link_protocol.sh src/proto_<new_version>_<new_hash>

This updates manifest/manifest.ml to add the new protocol.
Then, it runs make -C manifest to regenerate the relevant files.
"

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

new_version="$(basename "$1" | awk -F'_' '{print $2}')"
new_hash="$(basename "$1" | awk -F'_' '{print $3}')"

if [ -z "${new_version}" ] ; then
    echo "$usage"
    exit 1
fi

if [ -z "${new_hash}" ] ; then
    echo "$usage"
    exit 1
fi

# Copy the part of manifest/main.ml that is before the declaration of alpha.
echo "Updating manifest: copy header..."
grep -B 1000000 'let alpha = active Alpha "alpha"' manifest/main.ml | head --lines=-1 > manifest/main.ml.new

# Insert the new lines we want to insert.
echo "Updating manifest: add new line..."
echo "  let _${new_version}_${new_hash} = active (V ${new_version}) \"${new_hash}\"" >> manifest/main.ml.new
echo "" >> manifest/main.ml.new

# Copy the rest of the original file.
echo "Updating manifest: copy footer..."
grep -A 1000000 'let alpha = active Alpha "alpha"' manifest/main.ml >> manifest/main.ml.new

# Replace the original file.
echo "Updating manifest: replace file..."
mv manifest/main.ml.new manifest/main.ml

# Generate everything from the manifest.
echo "Updating manifest: generate dune and opam files..."
make -C manifest
