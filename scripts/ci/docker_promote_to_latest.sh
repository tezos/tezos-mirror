#!/bin/sh
set -eu

### Promote a specificied image to tag 'latest' (Docker default)

current_dir=$(cd "$(dirname "${0}")" && pwd)

ci_commit_tag=$(git tag --points-at HEAD)

export CI_COMMIT_TAG="${ci_commit_tag}"

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

## The goal of this script is to retag existing Docker images (do not rebuild them)
target_tag="${1:-latest}"

# shellcheck source=./scripts/releases/octez-release.sh
. "${2:-./scripts/releases/octez-release.sh}"

if [ -z "${gitlab_release}" ]; then
  echo "Error: could not find valid tag like *-vX.Y at branch HEAD"
  exit 1
fi

echo "### Promoting docker images with tag '${gitlab_release}' to tag '${target_tag}'"

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Merging tags for docker image: ${docker_image}"

  # Loop over architectures
  amends=''
  for docker_architecture in ${docker_architectures}; do
    docker pull "${docker_image}:${docker_architecture}_${gitlab_release}"
    amends="${amends} --amend ${docker_image}:${docker_architecture}_${gitlab_release}"
  done

  # Because of the variable amends, we use eval here to first construct the command
  # by concatenating all the arguments together (space separated), then read and execute it
  eval "docker manifest create ${docker_image}:${target_tag}${amends}"
  docker manifest push "${docker_image}:${target_tag}"
done
