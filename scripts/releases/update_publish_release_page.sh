#!/usr/bin/env bash

set -ex

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish assets and release page."
  exit 1
fi

if [ -z "${AWS_ACCESS_KEY_ID:-}" ] || [ -z "${AWS_SECRET_ACCESS_KEY:-}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

# octez's versions.json lives at the root of "<S3_BUCKET><BUCKET_PATH>/" (no
# component subdirectory, unlike the other components).
remote="s3://${S3_BUCKET}${BUCKET_PATH:-}/versions.json"

echo "Downloading versions.json..."
aws s3 cp "${remote}" "./versions.json" --region "${REGION}"

echo "Before:"
cat "./versions.json"

# Migrate to the new versions.json schema using version_manager itself, the
# source of truth for the format: its parser still reads the old flat "rc"/
# "beta" integer fields, and its serializer always writes the new canonical
# form, where a prerelease is a nested object { "kind", "number" }.
#
# [set-inactive] is used as a no-op load+save: the predicate (v0.0) matches no
# version, so no flag is modified, but the file is reparsed and rewritten in
# the new format. Note this also re-emits the canonical [active]/[latest]/
# [pubDate] fields with their defaults for any entry that lacked them.
echo "Building version_manager..."
dune build ci/bin_release_page/src/
VM="_build/default/ci/bin_release_page/src/version_manager.exe"

echo "Migrating versions.json to the new format..."
"$VM" --file ./versions.json set-inactive --major 0 --minor 0

echo "After:"
cat "./versions.json"

echo "Uploading versions.json..."
if aws s3 cp "./versions.json" "${remote}" --region "${REGION}"; then
  echo "Upload of versions.json successful!"
else
  echo "Upload of versions.json failed. Please check the configuration and try again."
  exit 1
fi
