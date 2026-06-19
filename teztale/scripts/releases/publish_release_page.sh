#!/usr/bin/env bash

# Publishes the Teztale release page: renders the HTML page from the published
# versions.json, then uploads it to S3 and invalidates the CDN.
#
# The release assets and versions.json are deployed beforehand by
# [deploy_release_page_assets.sh]. This script only reflects what is already
# published, so it can be re-run at any time to regenerate the page without
# re-running a release.

set -eu

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish the release page."
  exit 1
fi

if [ -z "${DISTRIBUTION_ID:-}" ]; then
  echo "DISTRIBUTION_ID variable is not set, impossible to create an invalidation."
  exit 1
fi

if [ -z "${AWS_ACCESS_KEY_ID:-}" ] || [ -z "${AWS_SECRET_ACCESS_KEY:-}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

dune build ci/bin_release_page/src/

VM="_build/default/ci/bin_release_page/src/version_manager.exe"
S3_PATH="${S3_BUCKET}${BUCKET_PATH:-}/teztale"

# Download the published versions.json so the page reflects what is published.
echo "Downloading versions.json..."
$VM download --path "${S3_PATH}"

echo "Building release page"
dune exec ./ci/bin_release_page/src/release_page.exe -- --component 'teztale' \
  --title 'Teztale releases' --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path \
  "${BUCKET_PATH:-}" binaries packages

echo "Syncing files to remote s3 bucket"
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}${BUCKET_PATH:-}/" --region "${REGION}" && aws s3 cp "./index.html" "s3://${S3_BUCKET}${BUCKET_PATH:-}/teztale/" --region "${REGION}"; then
  echo "Deployment successful!"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create an invalidation so that the web page actually updates.
aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
