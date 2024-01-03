#!/bin/sh
set -eu

### Promote a specificied image to tag 'latest' (Docker default)

current_dir=$(cd "$(dirname "${0}")" && pwd)

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

## The goal of this script is to retag existing Docker images (do not rebuild them)
target_tag='latest'

# Find tag with format vX.Y at branch HEAD
tag_to_promote=$(git tag --points-at HEAD | grep -oE '^v[0-9]{1,3}\.[0-9]{1,3}$' || :)

if [ -z "${tag_to_promote}" ]; then
  echo "Error: could not find valid tag like vX.Y at branch HEAD"
  exit 1
fi

echo "### Promoting docker images with tag '${tag_to_promote}' to tag '${target_tag}'"

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Merging tags for docker image: ${docker_image}"

  # Loop over architectures
  amends=''
  for docker_architecture in ${docker_architectures}; do
    docker pull "${docker_image}:${docker_architecture}_${tag_to_promote}"
    amends="${amends} --amend ${docker_image}:${docker_architecture}_${tag_to_promote}"
  done

  # Because of the variable amends, we use eval here to first construct the command
  # by concatenating all the arguments together (space separated), then read and execute it
  eval "docker manifest create ${docker_image}:${target_tag}${amends}"
  docker manifest push "${docker_image}:${target_tag}"
done
