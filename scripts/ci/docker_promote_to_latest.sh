#!/bin/sh
set -eu

### Promote a specified image to a target tag (default: latest).
### Wrapper around docker_promote.sh for Octez release pipelines.
###
### Usage: docker_promote_to_latest.sh [TARGET_TAG [RELEASE_SCRIPT]]

current_dir=$(cd "$(dirname "${0}")" && pwd)

ci_commit_tag=$(git tag --points-at HEAD)
export CI_COMMIT_TAG="${ci_commit_tag}"

target_tag="${1:-latest}"
release_script="${2:-./scripts/releases/octez-release.sh}"

# shellcheck source=./scripts/releases/octez-release.sh
. "${release_script}"

if [ -z "${gitlab_release}" ]; then
  echo "Error: could not find valid tag like *-vX.Y at branch HEAD"
  exit 1
fi

exec "${current_dir}/docker-promote.sh" --source-tag "${gitlab_release}" --target-tag "${target_tag}" --skip-per-arch-tags
