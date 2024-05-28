#!/usr/bin/env bash

set -eu

# shellcheck disable=SC2207
export STS=($(aws sts assume-role-with-web-identity \
  --role-arn "${ROLE_ARN}" \
  --role-session-name "GitLabRunner-${CI_PROJECT_ID}-${CI_PIPELINE_ID}" \
  --web-identity-token "${CI_JOB_JWT_V2}" \
  --duration-seconds 3600 \
  --query 'Credentials.[AccessKeyId,SecretAccessKey,SessionToken]' \
  --output text))

export AWS_ACCESS_KEY_ID="${STS[0]}"
export AWS_SECRET_ACCESS_KEY="${STS[1]}"
export AWS_SESSION_TOKEN="${STS[2]}"

aws sts get-caller-identity

if [ "$PACKAGE_FORMAT" = "deb" ]; then
  # Update debian repository
  deb-s3 upload --s3-region "${AWS_BUCKET_REGION}" \
    --codename "${CODENAME}" \
    --arch "${ARCH}" \
    --bucket "${OCTEZ_DEB_BUCKET}" dist/debian/*.deb
else
  # Update rpm repository
  #
  # We're fetching the current state of the bucket, we're updating it with new packages
  # and run createrepo to update the index.
  #
  aws s3 sync s3://"${OCTEZ_RPM_BUCKET}" rpm-bucket
  mkdir -p rpm-bucket/"${CODENAME}"
  cp dist/fedora/*.rpm rpm-bucket/"${CODENAME}"/
  createrepo rpm-bucket/"${CODENAME}"
  aws s3 sync rpm-bucket s3://"${OCTEZ_RPM_BUCKET}"
fi
