#!/usr/bin/env bash

set -ex

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish assets and release page."
  exit 1
fi

if [ -z "${AWS_ACCESS_KEY_ID}" ] || [ -z "${AWS_SECRET_ACCESS_KEY}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

echo "Downloading versions.json..."
aws s3 cp "s3://${S3_BUCKET}${BUCKET_PATH:-}/versions.json" "./versions.json" --region "${REGION}"

echo "Before:"
cat "./versions.json"

echo "Removing all v25 entries from versions.json..."
jq '[.[] | select(.major != 25)]' ./versions.json > ./tmp.json
mv ./tmp.json ./versions.json

echo "After:"
cat "./versions.json"

echo "Uploading versions.json..."
if aws s3 cp "./versions.json" "s3://${S3_BUCKET}${BUCKET_PATH:-}/versions.json" --region "${REGION}"; then
  echo "Upload of versions.json successful!"
else
  echo "Upload of versions.json failed. Please check the configuration and try again."
  exit 1
fi
