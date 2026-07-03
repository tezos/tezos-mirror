#!/usr/bin/env bash

# Publishes the Octez release page: renders the HTML page and RSS feed from the
# published versions.json, then uploads them to S3 and invalidates the CDN.
#
# The release assets and versions.json are deployed beforehand by
# [deploy-release-page-assets.sh]. This script only reflects what is already
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
S3_PATH="${S3_BUCKET}${BUCKET_PATH:-}"

# Download the published versions.json so the page reflects what is published.
echo "Downloading versions.json..."
$VM download --path "${S3_PATH}"

# Generate the release page HTML and RSS feed
./scripts/releases/generate-release-page.sh

# Upload HTML, CSS and RSS feed to S3
echo "Syncing html files to remote s3 bucket..."
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}${BUCKET_PATH:-}/" --cache-control "max-age=30, must-revalidate" --region "${REGION}" &&
  aws s3 cp "./index.html" "s3://${S3_BUCKET}${BUCKET_PATH:-}/" --region "${REGION}" &&
  aws s3 cp "./older_releases.html" "s3://${S3_BUCKET}${BUCKET_PATH:-}/" --region "${REGION}" &&
  aws s3 cp "./feed.xml" "s3://${S3_BUCKET}${BUCKET_PATH:-}/" --region "${REGION}"; then
  echo "Deployment successful! Release page available at: ${URL:-https://${S3_BUCKET}}${BUCKET_PATH:-}"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create a CloudFront invalidation so that the web page actually updates.
aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
