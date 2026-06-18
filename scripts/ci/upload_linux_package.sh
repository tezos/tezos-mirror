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

# Update debian repository
deb-s3 upload --s3-region "${AWS_BUCKET_REGION}" \
  --codename "${CODENAME}" \
  --arch "${ARCH}" \
  --bucket "${OCTEZ_DEB_BUCKET}" dist/debian/*.deb
