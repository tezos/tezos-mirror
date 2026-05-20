#!/bin/sh
set -eu

### Promote a packaging revision image to its canonical version tag.
### Wrapper around docker_promote.sh for packaging revision pipelines.
###
### e.g., octez-v24.0-1 -> octez-v24.0

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Validate that CI_COMMIT_TAG is set and matches packaging revision format
if [ -z "${CI_COMMIT_TAG}" ]; then
  echo "Error: CI_COMMIT_TAG is not set"
  exit 1
fi

. scripts/releases/octez-release.sh

# Validate packaging revision format (octez-vX.Y-N)
if [ -z "${gitlab_packaging_revision_version}" ]; then
  echo "Error: CI_COMMIT_TAG '${CI_COMMIT_TAG}' does not match packaging revision format (octez-vX.Y-N)"
  exit 1
fi

base_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-v[0-9]+\.[0-9]+)-[0-9]+$/\1/p')

if [ -z "${base_version}" ]; then
  echo "Error: could not extract base version from '${CI_COMMIT_TAG}'"
  exit 1
fi

exec "${current_dir}/docker-promote.sh" --source-tag "${CI_COMMIT_TAG}" --target-tag "${base_version}"
