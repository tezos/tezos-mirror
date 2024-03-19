#!/bin/sh
set -eu

### Create a GitLab Etherlink release with links to all the related resources

# shellcheck source=./scripts/ci/octez-release.sh
. ./scripts/ci/etherlink-release.sh

release-cli create \
  --name="${gitlab_release_name}" \
  --tag-name="${CI_COMMIT_TAG}"
