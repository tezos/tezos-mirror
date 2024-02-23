#!/bin/sh
set -eu

# GitLab Release command-line tool
# https://gitlab.com/gitlab-org/release-cli

# Enable release-cli verbose mode
export DEBUG='true'

kernels_artifact_url="$CI_JOB_URL/artifacts/raw/kernels.tar.gz"

release-cli create \
  --name="Release ${CI_COMMIT_TAG}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --assets-link="{\"name\":\"Kernels\",\"url\":\"${kernels_artifact_url}\"}"
