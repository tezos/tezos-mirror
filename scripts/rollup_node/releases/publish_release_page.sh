#!/usr/bin/env bash

set -e

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish assets and release page."
  exit 1
fi

if [ -z "${DISTRIBUTION_ID:-}" ]; then
  echo "DISTRIBUTION_ID variable is not set, impossible to create an invalidation."
  exit 1
fi

# We use a file to list versions so that we can control what is actually displayed.
versions_list_filename="versions.json"

if [ -z "${AWS_ACCESS_KEY_ID}" ] || [ -z "${AWS_SECRET_ACCESS_KEY}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

aws s3 cp s3://"${S3_BUCKET}""${BUCKET_PATH}"/octez-smart-rollup-node/"$versions_list_filename" "./$versions_list_filename"

# If it's a release, we actually push the assets to the s3 bucket
if [ -n "${CI_COMMIT_TAG}" ]; then

  # Initialize rollup-node release variables needed later to determine relevant version information:
  # - major and minor version numbers (resp. [release_major_version], [release_minor_version])
  # - and, optionally, release candidate version number ([release_rc_version]).
  # shellcheck source=./scripts/rollup_node/releases/release.sh
  . ./scripts/rollup_node/releases/release.sh

  if [ -z "${release}" ]; then
    echo "This is not a Rollup-node release. No assets will be added to the release page."
  else

    # Add the new version to the $versions_list_filename JSON file.
    # Since jq cannot modify the file in-place, we write to a temporary file first.
    # [release_rc_version], [release_major_version] and [release_minor_version] defined in [./scripts/releases/octez-release.sh]
    if [ -n "${release_rc_version}" ]; then
      rc="${release_rc_version}"
      jq ". += [{\"major\":${release_major_version}, \"minor\":${release_minor_version},\"rc\":${rc}}]" "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"
    else
      # This is a release, we assume it's the latest.
      # All the others are marked [latest = false].
      jq 'map(.latest = false)' "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"
      jq ". += [{\"major\":${release_major_version}, \"minor\":${release_minor_version}, \"latest\":true}]" "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"
    fi

    # Upload binaries to S3 bucket
    echo "Uploading binaries..."
    aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/x86_64/" --region "${REGION}"
    aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/arm64/" --region "${REGION}"

    # Create and push archives
    tar -czf "${release}.tar.gz" --transform 's|^\octez-binaries/x86_64/|octez/|' octez-binaries/x86_64/*
    aws s3 cp "./${release}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/x86_64/" --region "${REGION}"
    sha256sum "${release}.tar.gz" >> "./x86_64_sha256sums.txt"
    tar -czf "${release}.tar.gz" --transform 's|^\octez-binaries/arm64/|octez/|' octez-binaries/arm64/*
    sha256sum "${release}.tar.gz" >> "./arm64_sha256sums.txt"
    aws s3 cp "./${release}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/arm64/" --region "${REGION}"

    # Push checksums for x86_64 binaries
    echo "Generating checksums for x86_64 binaries"
    for binary in ./octez-binaries/x86_64/*; do
      filename=$(basename "$binary")
      [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./x86_64_sha256sums.txt"
    done
    aws s3 cp "./x86_64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/x86_64/sha256sums.txt"

    # Push checksums for arm64 binaries
    echo "Generating checksums for arm64 binaries"
    for binary in ./octez-binaries/arm64/*; do
      filename=$(basename "$binary")
      [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./arm64_sha256sums.txt"
    done
    aws s3 cp "./arm64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/octez-smart-rollup-node-v${release_no_v}/binaries/arm64/sha256sums.txt"

  fi
else
  echo "No tag found. No asset will be added to the release page."
fi

echo "Syncing $versions_list_filename to remote s3 bucket"
aws s3 cp "./$versions_list_filename" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/" --region "${REGION}"

echo "Building release page"
dune exec ./ci/bin_release_page/release_page.exe -- --component 'octez-smart-rollup-node' \
  --title 'Octez Smart Rollup node releases' --bucket "${S3_BUCKET}" --path \
  "${BUCKET_PATH:-}" binaries

echo "Syncing html files to remote s3 bucket"
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}${BUCKET_PATH}/" --cache-control "max-age=30, must-revalidate" --region "${REGION}" && aws s3 cp "./index.html" "s3://${S3_BUCKET}${BUCKET_PATH}/octez-smart-rollup-node/" --region "${REGION}"; then
  echo "Deployment successful!"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create an invalidation so that the web page actually updates.

aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
