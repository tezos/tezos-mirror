#!/bin/sh
set -eu

### Create a GitLab Etherlink release with links to all the related resources

# shellcheck source=./scripts/ci/etherlink-release.sh
. ./scripts/ci/etherlink-release.sh

kernels_artifact_url="$CI_JOB_URL/artifacts/raw/kernels.tar.gz"

release-cli create \
  --name="${gitlab_release_name}" \
  --tag-name="${CI_COMMIT_TAG}" \
  --assets-link="{\"name\":\"Kernels\",\"url\":\"${kernels_artifact_url}\"}"
