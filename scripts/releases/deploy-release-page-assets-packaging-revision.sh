#!/usr/bin/env bash

# Deploys the Octez release assets for a packaging revision (octez-vX.Y-N):
# updates the build number in versions.json (the source of truth) and re-uploads
# the binaries, checksums and archives to S3.
#
# It does NOT generate or publish the release page itself: that is done by
# [publish-release-page.sh], which renders the page from the versions.json
# published here.

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

# Initialize octez release variables:
# - [gitlab_release_major_version], [gitlab_release_minor_version]
# - [gitlab_packaging_revision_version]
# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

if [ -z "${gitlab_packaging_revision_version}" ]; then
  echo "This is not a packaging revision tag. Exiting."
  exit 1
fi

if [ -z "${gitlab_release_major_version}" ] || [ -z "${gitlab_release_minor_version}" ]; then
  echo "Could not parse major/minor version from tag ${CI_COMMIT_TAG}. Exiting."
  exit 1
fi

# The S3 directory name for this version, e.g. "octez-v20.0"
version_dir="octez-v${gitlab_release_major_version}.${gitlab_release_minor_version}"

dune build ci/bin_release_page/src/

VM="_build/default/ci/bin_release_page/src/version_manager.exe"
S3_PATH="${S3_BUCKET}${BUCKET_PATH:-}"

echo "Processing packaging revision ${gitlab_packaging_revision_version} for ${version_dir}..."

# Download versions.json from remote storage
echo "Downloading versions.json..."
$VM download --path "${S3_PATH}"

# Update the build number in versions.json
echo "Updating build number to ${gitlab_packaging_revision_version}..."
$VM \
  update-build-number \
  --major "${gitlab_release_major_version}" \
  --minor "${gitlab_release_minor_version}" \
  --build-number "${gitlab_packaging_revision_version}"

# Upload binaries to S3 bucket (overwriting existing version's binaries)
echo "Uploading binaries..."
aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}${BUCKET_PATH:-}/${version_dir}/binaries/x86_64/" --region "${REGION}"
aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}${BUCKET_PATH:-}/${version_dir}/binaries/arm64/" --region "${REGION}"

# Create and push archives
tar -czf "${version_dir}.tar.gz" --transform 's|^octez-binaries/x86_64/|octez/|' octez-binaries/x86_64/*
aws s3 cp "./${version_dir}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH:-}/${version_dir}/binaries/x86_64/" --region "${REGION}"
sha256sum "${version_dir}.tar.gz" >> "./x86_64_sha256sums.txt"
tar -czf "${version_dir}.tar.gz" --transform 's|^octez-binaries/arm64/|octez/|' octez-binaries/arm64/*
sha256sum "${version_dir}.tar.gz" >> "./arm64_sha256sums.txt"
aws s3 cp "./${version_dir}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH:-}/${version_dir}/binaries/arm64/" --region "${REGION}"

# Push checksums for x86_64 binaries
echo "Generating checksums for x86_64 binaries"
for binary in ./octez-binaries/x86_64/*; do
  filename=$(basename "$binary")
  [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./x86_64_sha256sums.txt"
done
aws s3 cp "./x86_64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH:-}/${version_dir}/binaries/x86_64/sha256sums.txt"

# Push checksums for arm64 binaries
echo "Generating checksums for arm64 binaries"
for binary in ./octez-binaries/arm64/*; do
  filename=$(basename "$binary")
  [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./arm64_sha256sums.txt"
done
aws s3 cp "./arm64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH:-}/${version_dir}/binaries/arm64/sha256sums.txt"

# Upload versions.json back to remote storage
echo "Uploading versions.json..."
$VM upload --path "${S3_PATH}"
