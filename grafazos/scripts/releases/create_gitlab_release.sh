#!/bin/sh

### Create a GitLab release with links to all the related resources

if [ -z "${CI_COMMIT_TAG:-}" ]; then
  echo "CI_COMMIT_TAG is not set, impossible to create a gitlab release page."
  exit 1
fi

# shellcheck source=./grafazos/scripts/releases/release.sh
. ./grafazos/scripts/releases/release.sh

set -eu

# GitLab Release command-line tool
# https://gitlab.com/gitlab-org/release-cli

# Enable release-cli verbose mode
export DEBUG='true'

release-cli create \
  --name="${release_name}" \
  --tag-name="${CI_COMMIT_TAG}"
