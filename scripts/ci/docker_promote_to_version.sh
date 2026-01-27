#!/bin/sh
set -eu

### Promote a packaging revision image to version tag (remove revision suffix)
### e.g., octez-v19.0-1 -> octez-v19.0

current_dir=$(cd "$(dirname "${0}")" && pwd)

ci_commit_tag=$(git tag --points-at HEAD)

export CI_COMMIT_TAG="${ci_commit_tag}"

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

## Extract base version from packaging revision tag
# octez-vX.Y-N -> octez-vX.Y
base_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-v[0-9]+\.[0-9]+)-[0-9]+$/\1/p')

if [ -z "${base_version}" ]; then
  echo "Error: CI_COMMIT_TAG '${CI_COMMIT_TAG}' does not match packaging revision format (octez-vX.Y-N)"
  exit 1
fi

echo "### Promoting docker images from '${CI_COMMIT_TAG}' to '${base_version}'"

# In test mode, we use GitLab registry; in production, we use Docker Hub
# This is determined by the docker image name prefix set in docker_registry_auth.sh
echo "Using docker registry: ${DOCKER_IMAGE_NAME}"

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Promoting docker image: ${docker_image}"

  # Loop over architectures
  amends=''
  for docker_architecture in ${docker_architectures}; do
    source_tag="${docker_architecture}_${CI_COMMIT_TAG}"
    echo "Pulling ${docker_image}:${source_tag}"
    docker pull "${docker_image}:${source_tag}"

    # Verify signature before amend (only in production)
    if echo "${DOCKER_IMAGE_NAME}" | grep -q "registry.gitlab.com"; then
      echo "Skipping signature verification in test mode"
    else
      ./scripts/ci/docker_verify_signature.sh "${docker_image}:${source_tag}"
    fi

    amends="${amends} --amend ${docker_image}:${source_tag}"
  done

  # Because of the variable amends, we use eval here to first construct the command
  # by concatenating all the arguments together (space separated), then read and execute it
  target_tag="${base_version}"
  eval "docker manifest create ${docker_image}:${target_tag}${amends}"
  docker manifest push "${docker_image}:${target_tag}"

  # Sign image (only in production)
  if echo "${DOCKER_IMAGE_NAME}" | grep -q "registry.gitlab.com"; then
    echo "Skipping signing in test mode"
  else
    ./scripts/ci/docker_sign.sh "${docker_image}:${target_tag}"
  fi
done

# Signature verification (only in production)
if echo "${DOCKER_IMAGE_NAME}" | grep -q "registry.gitlab.com"; then
  echo "Skipping signature verification in test mode"
else
  for docker_image in ${docker_images}; do
    echo "### Verifying signature for docker image: ${docker_image}"
    ./scripts/ci/docker_verify_signature.sh "${docker_image}:${target_tag}"
  done
fi

echo "### Successfully promoted images to tag '${base_version}'"
