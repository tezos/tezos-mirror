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

aws s3 cp s3://"${S3_BUCKET}"/grafazos/"$versions_list_filename" "./$versions_list_filename"

# If it's a release, we actually push the assets to the s3 bucket
if [ -n "${CI_COMMIT_TAG}" ]; then

  # Initialize octez release variables needed later to determine relevant version information:
  # - major and minor version numbers (resp. [release_major_version], [release_minor_version])
  # - and, optionally, release candidate version number ([release_rc_version]).
  # shellcheck source=./grafazos/scripts/releases/release.sh
  . ./grafazos/scripts/releases/release.sh

  if [ -z "${release}" ]; then
    echo "This is not a Grafazos release. No assets will be added to the release page."
  else

    # Add the new version to the $versions_list_filename JSON file.
    # Since jq cannot modify the file in-place, we write to a temporary file first.
    # [release_rc_version], [release_major_version] and [release_minor_version]
    # defined in [./grafazos/scripts/releases/release.sh]
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
    echo "Uploading dashboards..."
    aws s3 sync "./grafazos/output/" "s3://${S3_BUCKET}/grafazos/grafazos-v${release_no_v}/dashboards/" --exclude "*" --include "*.json" --region "${REGION}"

    # Create and push archives
    tar -czf "grafazos-v${release_no_v}.tar.gz" --transform 's|output/*||' --exclude ".keep" grafazos/output/
    aws s3 cp "./grafazos-v${release_no_v}.tar.gz" "s3://${S3_BUCKET}/grafazos/grafazos-v${release_no_v}/dashboards/" --region "${REGION}"
    sha256sum "grafazos-v${release_no_v}.tar.gz" >> "./sha256sums.txt"

    # Push checksums for dashboards
    echo "Generating checksums for dashboards"
    for dashboard in ./grafazos/output/*.json ./grafazos/output/**/*.json; do
      filename=$(basename "$dashboard")
      [ -f "$dashboard" ] && sha256sum "$dashboard" | awk -v name="$filename" '{print $1, name}' >> "./sha256sums.txt"
    done
    aws s3 cp "./sha256sums.txt" "s3://${S3_BUCKET}/grafazos/grafazos-v${release_no_v}/dashboards/sha256sums.txt"
  fi
else
  echo "No tag found. No asset will be added to the release page."
fi

echo "Syncing $versions_list_filename to remote s3 bucket"
if aws s3 cp "./$versions_list_filename" "s3://${S3_BUCKET}/grafazos/" --region "${REGION}"; then
  echo "Deployment of ${versions_list_filename} successful!"
else
  echo "Deployment of ${versions_list_filename} failed. Please check the configuration and try again."
  exit 1
fi

echo "Building release page"
dune exec ./ci/bin_release_page/release_page.exe -- --component 'grafazos' \
  --title 'Grafazos releases' --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path \
  "${BUCKET_PATH:-}" dashboards

echo "Syncing files to remote s3 bucket"
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}/" --region "${REGION}" && aws s3 cp "./index.html" "s3://${S3_BUCKET}/grafazos/" --region "${REGION}"; then
  echo "Deployment successful!"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create an invalidation so that the web page actually updates.

aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
