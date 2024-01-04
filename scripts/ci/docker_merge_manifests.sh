#!/bin/sh
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

short_tag="${DOCKER_IMAGE_TAG}"

last_commit_date_time=$(git log --pretty=format:"%cd" -1 --date="format:%Y%m%d%H%M%S" 2>&1)
# Format: tag_1234abcd_YYYYMMDDHHMMSS
long_tag="${short_tag}_${CI_COMMIT_SHORT_SHA}_${last_commit_date_time}"

docker_tags="${short_tag} ${long_tag}"

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Merging tags for docker image: ${docker_image}"

  # Loop over architectures
  amends=''
  for docker_architecture in ${docker_architectures}; do
    amends="${amends} --amend ${docker_image}:${docker_architecture}_${short_tag}"
  done

  # Loop over tags
  for docker_tag in ${docker_tags}; do
    # Because of the variable amends, we use eval here to first construct the command
    # by concatenating all the arguments together (space separated), then read and execute it
    eval "docker manifest create ${docker_image}:${docker_tag}${amends}"
    docker manifest push "${docker_image}:${docker_tag}"
  done
done
