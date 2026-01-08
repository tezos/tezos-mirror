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

if [ -z "${AWS_ACCESS_KEY_ID}" ] || [ -z "${AWS_SECRET_ACCESS_KEY}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

# If it's a release, we actually push the assets to the s3 bucket
if [ -n "${CI_COMMIT_TAG}" ]; then

  # Initialize octez release variables needed later to determine relevant version information:
  # - major and minor version numbers (resp. [gitlab_release_major_version], [gitlab_release_minor_version])
  # - and, optionally, release candidate version number ([gitlab_release_rc_version]).
  # shellcheck source=./scripts/releases/octez-release.sh
  . ./scripts/releases/octez-release.sh

  if [ -z "${gitlab_release}" ]; then
    echo "This is not an Octez release. No assets will be added to the release page."
  else

    announcement="https://octez.tezos.com/docs/releases/version-${gitlab_release_major_version}.html"

    # Add the new version using version_manager
    # [gitlab_release_rc_version], [gitlab_release_major_version] and [gitlab_release_minor_version] defined in [./scripts/releases/octez-release.sh]
    echo "Adding version ${gitlab_release} to release page..."
    dune exec ./ci/bin_release_page/version_manager.exe -- \
      --path "${S3_BUCKET}${BUCKET_PATH}" \
      add \
      --major "${gitlab_release_major_version}" \
      --minor "${gitlab_release_minor_version}" \
      ${gitlab_release_rc_version:+--rc "${gitlab_release_rc_version}"} \
      --announcement "${announcement}"

    # Set as latest only if not an RC
    if [ -z "${gitlab_release_rc_version}" ]; then
      echo "Setting version as latest..."
      dune exec ./ci/bin_release_page/version_manager.exe -- \
        --path "${S3_BUCKET}${BUCKET_PATH}" \
        set-latest \
        --major "${gitlab_release_major_version}" \
        --minor "${gitlab_release_minor_version}"
    fi

    # Active versions logic
    if [ -z "${gitlab_release_rc_version}" ] && [ "${gitlab_release_minor_version}" = "0" ]; then

      echo "Major release (${gitlab_release_major_version}.0), set all previous versions as not active..."

      prev_major=$((gitlab_release_major_version - 1))
      if [ "$prev_major" -ge 0 ]; then
        dune exec ./ci/bin_release_page/version_manager.exe -- \
          --path "${S3_BUCKET}${BUCKET_PATH}" \
          set-inactive \
          --major "${prev_major}" || true
      fi
    fi

    echo "Set versions ${gitlab_release_major_version} as active..."
    dune exec ./ci/bin_release_page/version_manager.exe -- \
      --path "${S3_BUCKET}${BUCKET_PATH}" \
      set-active \
      --major "${gitlab_release_major_version}"

    # Upload binaries to S3 bucket
    echo "Uploading binaries..."
    aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}${BUCKET_PATH}/${gitlab_release}/binaries/x86_64/" --region "${REGION}"
    aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}${BUCKET_PATH}/${gitlab_release}/binaries/arm64/" --region "${REGION}"

    # Create and push archives
    tar -czf "${gitlab_release}.tar.gz" --transform 's|^\octez-binaries/x86_64/|octez/|' octez-binaries/x86_64/*
    aws s3 cp "./${gitlab_release}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH}/${gitlab_release}/binaries/x86_64/" --region "${REGION}"
    sha256sum "${gitlab_release}.tar.gz" >> "./x86_64_sha256sums.txt"
    tar -czf "${gitlab_release}.tar.gz" --transform 's|^\octez-binaries/arm64/|octez/|' octez-binaries/arm64/*
    sha256sum "${gitlab_release}.tar.gz" >> "./arm64_sha256sums.txt"
    aws s3 cp "./${gitlab_release}.tar.gz" "s3://${S3_BUCKET}${BUCKET_PATH}/${gitlab_release}/binaries/arm64/" --region "${REGION}"

    # Push checksums for x86_64 binaries
    echo "Generating checksums for x86_64 binaries"
    for binary in ./octez-binaries/x86_64/*; do
      filename=$(basename "$binary")
      [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./x86_64_sha256sums.txt"
    done
    aws s3 cp "./x86_64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH}/${gitlab_release}/binaries/x86_64/sha256sums.txt"

    # Push checksums for arm64 binaries
    echo "Generating checksums for arm64 binaries"
    for binary in ./octez-binaries/arm64/*; do
      filename=$(basename "$binary")
      [ -f "$binary" ] && sha256sum "$binary" | awk -v name="$filename" '{print $1, name}' >> "./arm64_sha256sums.txt"
    done
    aws s3 cp "./arm64_sha256sums.txt" "s3://${S3_BUCKET}${BUCKET_PATH}/${gitlab_release}/binaries/arm64/sha256sums.txt"

  fi
else
  echo "No tag found. No asset will be added to the release page."
fi

# Generate RSS feed for all releases
echo "Generating RSS feed..."
dune exec ./ci/bin_release_page/version_manager.exe -- \
  --path "${S3_BUCKET}${BUCKET_PATH}" \
  generate-rss \
  --base-url "${URL:-https://${S3_BUCKET}}"

echo "Building older releases page (inactive versions only)"
dune exec ./ci/bin_release_page/release_page.exe -- --component 'octez' \
  --title 'Octez older releases' --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path \
  "${BUCKET_PATH:-}" --filter-active inactive changelog binaries packages

# Rename the second page to older_releases.html
mv index.html older_releases.html

# Generate the main page again (active versions) to create index.html
echo "Generating main release page for index.html"
dune exec ./ci/bin_release_page/release_page.exe -- --component 'octez' \
  --title 'Octez releases' --bucket "${S3_BUCKET}" --url "${URL:-${S3_BUCKET}}" --path \
  "${BUCKET_PATH:-}" --filter-active active changelog binaries packages

echo "Syncing html files to remote s3 bucket"
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}${BUCKET_PATH}/" --cache-control "max-age=30, must-revalidate" --region "${REGION}" &&
  aws s3 cp "./index.html" "s3://${S3_BUCKET}${BUCKET_PATH}/" --region "${REGION}" &&
  aws s3 cp "./older_releases.html" "s3://${S3_BUCKET}${BUCKET_PATH}/" --region "${REGION}" &&
  aws s3 cp "./feed.xml" "s3://${S3_BUCKET}${BUCKET_PATH}/" --region "${REGION}"; then
  echo "Deployment successful!"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create an invalidation so that the web page actually updates.

aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
