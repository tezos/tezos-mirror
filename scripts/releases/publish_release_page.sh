#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

REGION="${REGION:-eu-west-1}"

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish assets and release page."
  exit 1
fi

# We use a file to list releases so that we can control what is acutally displayed.
Releases_list="releases_list.txt"

if [ -n "${AWS_KEY_RELEASE_PUBLISH}" ] && [ -n "${AWS_SECRET_RELEASE_PUBLISH}" ]; then
  export AWS_ACCESS_KEY_ID="${AWS_KEY_RELEASE_PUBLISH}"
  export AWS_SECRET_ACCESS_KEY="${AWS_SECRET_RELEASE_PUBLISH}"
else
  echo "The AWS credentials are not found. Make sure AWS_KEY_RELEASE_PUBLISH and AWS_SECRET_RELEASE_PUBLISH are set."
  exit 1
fi

# If it's a release, we actually push the assets to the s3 bucket
if [ -n "${CI_COMMIT_TAG}" ]; then

  # shellcheck source=./scripts/ci/octez-release.sh
  . ./scripts/ci/octez-release.sh

  if [ -z "${gitlab_release}" ]; then
    echo "This is not an Octez release. No assets will be added to the release page."
  else

    #TODO: Add to docker image ?
    sudo apk add aws-cli gawk

    aws s3 cp s3://"${S3_BUCKET}"/"$Releases_list" "./$Releases_list"
    echo "${CI_COMMIT_TAG}" >> "./$Releases_list"

    # Upload binaries to S3 bucket
    echo "Uploading binaries..."
    aws s3 sync "./octez-binaries/x86_64/" "s3://${S3_BUCKET}/${gitlab_release}/binaries/x86_64/" --region "${REGION}"
    aws s3 sync "./octez-binaries/arm64/" "s3://${S3_BUCKET}/${gitlab_release}/binaries/arm64/" --region "${REGION}"

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

"${script_dir}"/create_release_page.sh "$Releases_list"

echo "Syncing files to remote s3 bucket"
if aws s3 cp "./docs/release_page/style.css" "s3://${S3_BUCKET}/" --region "${REGION}" && aws s3 cp "./index.html" "s3://${S3_BUCKET}/" --region "${REGION}" && aws s3 cp "./$Releases_list" "s3://${S3_BUCKET}/" --region "${REGION}"; then
  echo "Deployment successful!"
else
  echo "Deployment failed. Please check the configuration and try again."
  exit 1
fi

# Create an invalidation so that the web page actually updates.
# TODO: Allow to find the Distribution_id
#DISTRIBUTION_ID=$(aws cloudfront list-distributions --query "DistributionList.Items[?Aliases.Items[?contains(@, 'release-page-test.nomadic-labs.com')]].Id" --output text)
DISTRIBUTION_ID="E19JF46UG3Z747"
aws cloudfront create-invalidation --distribution-id "$DISTRIBUTION_ID" --paths "/*"
