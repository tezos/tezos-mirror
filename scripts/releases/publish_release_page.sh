#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

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

#TODO: Add to docker image ?
sudo apk add aws-cli

aws s3 cp s3://"${S3_BUCKET}"/"$versions_list_filename" "./$versions_list_filename"

# If it's a release, we actually push the assets to the s3 bucket
if [ -n "${CI_COMMIT_TAG}" ]; then

  # Initialize octez release variables needed later to determine relevant version information:
  # - major and minor version numbers (resp. [gitlab_release_major_version], [gitlab_release_minor_version])
  # - and, optionally, release candidate version number ([gitlab_release_rc_version]).
  # shellcheck source=./scripts/ci/octez-release.sh
  . ./scripts/ci/octez-release.sh

  if [ -z "${gitlab_release}" ]; then
    echo "This is not an Octez release. No assets will be added to the release page."
  else

    #TODO: Add to docker image ?
    sudo apk add gawk jq

    announcement="https://octez.tezos.com/docs/releases/version-${gitlab_release_major_version}.html"

    # Add the new version to the $versions_list_filename JSON file.
    # Since jq cannot modify the file in-place, we write to a temporary file first.
    # [gitlab_release_rc_version], [gitlab_release_major_version] and [gitlab_release_minor_version] defined in [./scripts/ci/octez-release.sh]
    if [ -n "${gitlab_release_rc_version}" ]; then
      rc="${gitlab_release_rc_version}"
      jq ". += [{\"major\":${gitlab_release_major_version}, \"minor\":${gitlab_release_minor_version},\"rc\":${rc},\"announcement\":\"${announcement}\"}]" "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"
    else
      # This is a release, we assume it's the latest.
      # All the others are marked [latest = false].
      jq 'map(.latest = false)' "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"
      jq ". += [{\"major\":${gitlab_release_major_version}, \"minor\":${gitlab_release_minor_version}, \"latest\":true, \"announcement\":\"${announcement}\"}]" "./${versions_list_filename}" > "./tmp.json" && mv "./tmp.json" "./${versions_list_filename}"
    fi

    # Upload binaries to S3 bucket
    echo "Uploading binaries..."
    aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/" --region "${REGION}"
    aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/" --region "${REGION}"

    # Create and push archives
    tar -czf "${gitlab_release}.tar.gz" --transform 's|^\octez-binaries/x86_64/|octez/|' octez-binaries/x86_64/*
    aws s3 cp "./${gitlab_release}.tar.gz" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/" --region "${REGION}"
    sha256sum "${gitlab_release}.tar.gz" >> "./x86_64_sha256sums.txt"
    tar -czf "${gitlab_release}.tar.gz" --transform 's|^\octez-binaries/arm64/|octez/|' octez-binaries/arm64/*
    sha256sum "${gitlab_release}.tar.gz" >> "./arm64_sha256sums.txt"
    aws s3 cp "./${gitlab_release}.tar.gz" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/" --region "${REGION}"

    # Push checksums for x86_64 binaries
    echo "Generating checksums for x86_64 binaries"
    for binary in ./octez-binaries/x86_64/*; do
      filename=$(basename "$binary")
      [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./x86_64_sha256sums.txt"
    done
    aws s3 cp "./x86_64_sha256sums.txt" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/sha256sums.txt"

    # Push checksums for arm64 binaries
    echo "Generating checksums for arm64 binaries"
    for binary in ./octez-binaries/arm64/*; do
      filename=$(basename "$binary")
      [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./arm64_sha256sums.txt"
    done
    aws s3 cp "./arm64_sha256sums.txt" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/sha256sums.txt"

  fi
else
  echo "No tag found. No asset will be added to the release page."
fi

"${script_dir}"/create_release_page.sh "$versions_list_filename"

echo "Syncing files to remote s3 bucket"
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}/" --region "${REGION}" && aws s3 cp "./index.html" "s3://${S3_BUCKET}/" --region "${REGION}" && aws s3 cp "./$versions_list_filename" "s3://${S3_BUCKET}/" --region "${REGION}"; then
  echo "Deployment successful!"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create an invalidation so that the web page actually updates.

aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
