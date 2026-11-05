#!/bin/sh
set -eu

### Promote a specified image to a target tag (default: latest).
### Wrapper around docker_promote.sh for Octez release pipelines.
###
### Usage: docker_promote_to_latest.sh [TARGET_TAG [RELEASE_SCRIPT]]

current_dir=$(cd "$(dirname "${0}")" && pwd)

# A tag pipeline already sets CI_COMMIT_TAG. Fall back to the tag at HEAD only
# for the suite's latest-release branch flow, where CI_COMMIT_TAG is unset. This
# avoids a multi-line value when several tags point at HEAD.
ci_commit_tag="${CI_COMMIT_TAG:-$(git tag --points-at HEAD)}"
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
