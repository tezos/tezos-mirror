#!/usr/bin/env bash

# Renders the whole release site and publishes it.
#
# Unlike the per-component [publish_release_page.sh] scripts, this renders every
# component's page at once from the built-in catalog in [release_page.ml],
# reading each component's published versions.json. It renders nothing that has
# not already been deployed: the assets and versions.json are deployed
# beforehand by the per-component [deploy_release_page_assets.sh] scripts, so
# this can be re-run at any time to regenerate the site without re-running a
# release.

set -eu

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish the release site."
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

dune build release_site/src/

RELEASE_PAGE="_build/default/release_site/src/release_page.exe"
VM="_build/default/release_site/src/version_manager.exe"
S3_PATH="${S3_BUCKET}${BUCKET_PATH:-}"

site_dir="$(mktemp -d)"

# Render every component's page from its published versions.json into [site_dir],
# mirroring the site layout (octez at the root, each other component under its
# own subdirectory).
echo "Rendering release site..."
$RELEASE_PAGE --site \
  --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path "${BUCKET_PATH:-}" \
  --output-dir "${site_dir}"

# Generate the octez RSS feed from the published octez versions.json.
echo "Generating RSS feed..."
$VM --file "${site_dir}/versions.json" download --path "${S3_PATH}"
$VM --file "${site_dir}/versions.json" generate-rss \
  --path "${S3_PATH}" --base-url "${URL:-https://${S3_BUCKET}}" \
  --output "${site_dir}/feed.xml"

# The stylesheet is shared by every page and lives at the root of the site.
cp "./docs/release_page/style.css" "${site_dir}/style.css"

echo "Syncing rendered site to remote S3 bucket..."
if aws s3 sync "${site_dir}/" "s3://${S3_PATH}/" --exclude "*.md" --region "${REGION}" &&
  aws s3 cp "${site_dir}/style.css" "s3://${S3_PATH}/style.css" \
    --cache-control "max-age=30, must-revalidate" --region "${REGION}"; then
  echo "Deployment successful! Release site available at: ${URL:-https://${S3_BUCKET}}${BUCKET_PATH:-}"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create a CloudFront invalidation so that the site actually updates.
aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
