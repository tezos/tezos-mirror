#!/bin/sh
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

# Loop over images
for docker_image in ${docker_images}; do
  docker push "${docker_image}:${DOCKER_IMAGE_TAG}"
done
