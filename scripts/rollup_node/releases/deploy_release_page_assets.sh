#!/usr/bin/env bash

# Deploys the Octez Smart Rollup node release assets: updates versions.json (the
# source of truth) and uploads the binaries, checksums and archives to S3.
#
# It does NOT render the release site itself: that is done by the site-wide
# [render_release_site.sh], which re-renders every component's page from the
# versions.json published here.

set -eu

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish assets."
  exit 1
fi

if [ -z "${AWS_ACCESS_KEY_ID:-}" ] || [ -z "${AWS_SECRET_ACCESS_KEY:-}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

# This job only runs on release-tag pipelines, so a tag must be present. If it
# is not, the job has been mis-wired: fail fast rather than silently round-trip
# versions.json.
if [ -z "${CI_COMMIT_TAG:-}" ]; then
  echo "CI_COMMIT_TAG is not set; this script must run on a release-tag pipeline."
  exit 1
fi

# Initialize rollup-node release variables needed later to determine relevant version information:
# - the date and the optional same-day counter ([release_date], [release_counter])
# - and, optionally, release candidate or beta version number ([release_rc_version], [release_beta_version]).
# shellcheck source=./scripts/rollup_node/releases/release.sh
. ./scripts/rollup_node/releases/release.sh

if [ -z "${release}" ]; then
  echo "This is not a Rollup-node release. No assets will be added to the release page."
  exit 0
fi

dune build release_site/src/

VM="_build/default/release_site/src/version_manager.exe"
S3_PATH="${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node"

echo "Downloading versions.json..."
$VM download --path "${S3_PATH}"

# Link to the changelog section of this release, with the anchor that Sphinx makes from its heading.
heading=$(awk -v h="Version ${release_no_v} " 'index($0, h) == 1 { print; exit }' docs/releases/smart_rollup_node_changelog.rst)
anchor=$(printf '%s' "${heading}" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/-/g; s/^-//; s/-$//')
ANNOUNCEMENT="https://octez.tezos.com/docs/releases/smart_rollup_node_changelog.html${anchor:+#${anchor}}"

# Pass --counter only when the tag carries one.
echo "Adding version ${release} to release page..."
$VM \
  add \
  --date "${release_date}" \
  ${release_counter:+--counter "${release_counter}"} \
  --announcement "${ANNOUNCEMENT}" \
  ${release_rc_version:+--rc "${release_rc_version}"} \
  ${release_beta_version:+--beta "${release_beta_version}"}

if [ -z "${release_rc_version}" ] && [ -z "${release_beta_version}" ]; then
  echo "Setting version as latest..."
  $VM \
    set-latest \
    --date "${release_date}" \
    ${release_counter:+--counter "${release_counter}"}
fi

# Upload binaries to S3 bucket
echo "Uploading binaries..."
aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/x86_64/" --region "${REGION}"
aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/arm64/" --region "${REGION}"

# Create and push archives
tar -czf "${release}.tar.gz" --transform 's|^\octez-binaries/x86_64/|octez/|' octez-binaries/x86_64/*
aws s3 cp "./${release}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/x86_64/" --region "${REGION}"
sha256sum "${release}.tar.gz" >> "./x86_64_sha256sums.txt"
tar -czf "${release}.tar.gz" --transform 's|^\octez-binaries/arm64/|octez/|' octez-binaries/arm64/*
sha256sum "${release}.tar.gz" >> "./arm64_sha256sums.txt"
aws s3 cp "./${release}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/arm64/" --region "${REGION}"

# Push checksums for x86_64 binaries
echo "Generating checksums for x86_64 binaries"
for binary in ./octez-binaries/x86_64/*; do
  filename=$(basename "$binary")
  [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./x86_64_sha256sums.txt"
done
aws s3 cp "./x86_64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/x86_64/sha256sums.txt"

# Push checksums for arm64 binaries
echo "Generating checksums for arm64 binaries"
for binary in ./octez-binaries/arm64/*; do
  filename=$(basename "$binary")
  [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./arm64_sha256sums.txt"
done
aws s3 cp "./arm64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH:-}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/arm64/sha256sums.txt"

echo "Uploading versions.json..."
$VM upload --path "${S3_PATH}"
