#!/usr/bin/env bash

# Generates the Octez release page HTML and RSS feed from a local versions.json.
# The caller is responsible for providing versions.json beforehand
# and for uploading the generated files afterwards.
#
# Usage: generate-release-page.sh [--version-path PATH]
#   --version-path PATH  Directory containing versions.json (default: ./).

set -eu

version_path="./"
while [ $# -gt 0 ]; do
  case "$1" in
  --version-path)
    shift
    version_path="$1"
    shift
    ;;
  *) shift ;;
  esac
done

VM="_build/default/ci/bin_release_page/src/version_manager.exe"
RELEASE_PAGE="_build/default/ci/bin_release_page/src/release_page.exe"
S3_BUCKET="${S3_BUCKET:-release-page-test.nomadic-labs.com}"
S3_PATH="${S3_BUCKET}${BUCKET_PATH:-}"

versions_file="${version_path%/}/versions.json"

# Generate RSS feed
echo "Generating RSS feed..."
$VM \
  --file "${versions_file}" \
  generate-rss \
  --path "${S3_PATH}" \
  --base-url "${URL:-https://${S3_BUCKET}}"

# Generate older releases page (inactive versions only)
echo "Building older releases page..."
$RELEASE_PAGE --component 'octez' \
  --title 'Octez older releases' --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path \
  "${BUCKET_PATH:-}" --filter-active inactive changelog binaries packages \
  --file "${versions_file}"
mv index.html older_releases.html

# Generate main release page (active versions only)
echo "Generating main release page..."
$RELEASE_PAGE --component 'octez' \
  --title 'Octez releases' --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path \
  "${BUCKET_PATH:-}" --filter-active active changelog binaries packages \
  --file "${versions_file}"
