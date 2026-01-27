#!/bin/sh
set -eu

### Promote a packaging revision image to version tag (remove revision suffix)
### e.g., octez-v24.0-1 -> octez-v24.0

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Validate that CI_COMMIT_TAG is set and matches packaging revision format
if [ -z "${CI_COMMIT_TAG}" ]; then
  echo "Error: CI_COMMIT_TAG is not set"
  exit 1
fi

# Validate packaging revision format (octez-vX.Y-N)
if ! echo "${CI_COMMIT_TAG}" | grep -qE '^octez-v[0-9]+\.[0-9]+-[0-9]+$'; then
  echo "Error: CI_COMMIT_TAG '${CI_COMMIT_TAG}' does not match packaging revision format (octez-vX.Y-N)"
  exit 1
fi

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

## Extract base version from packaging revision tag
# octez-vX.Y-N -> octez-vX.Y
base_version=$(echo "${CI_COMMIT_TAG}" | sed -nE 's/^(octez-v[0-9]+\.[0-9]+)-[0-9]+$/\1/p')

echo "### Promoting docker images from '${CI_COMMIT_TAG}' to '${base_version}'"

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Promoting docker image: ${docker_image}"

  # First, promote individual architecture images
  for docker_architecture in ${docker_architectures}; do
    source_arch_tag="${docker_architecture}_${CI_COMMIT_TAG}"
    target_arch_tag="${docker_architecture}_${base_version}"

    echo "Pulling ${docker_image}:${source_arch_tag}"
    docker pull "${docker_image}:${source_arch_tag}"

    # Verify signature before promoting
    ./scripts/ci/docker_verify_signature.sh "${docker_image}:${source_arch_tag}"

    echo "Tagging ${docker_image}:${source_arch_tag} -> ${docker_image}:${target_arch_tag}"
    docker tag "${docker_image}:${source_arch_tag}" "${docker_image}:${target_arch_tag}"
    docker push "${docker_image}:${target_arch_tag}"

    # Sign individual architecture image
    ./scripts/ci/docker_sign.sh "${docker_image}:${target_arch_tag}"
  done

  # Then, create multi-arch manifest using the newly tagged images
  amends=''
  for docker_architecture in ${docker_architectures}; do
    target_arch_tag="${docker_architecture}_${base_version}"
    amends="${amends} --amend ${docker_image}:${target_arch_tag}"
  done

  # Because of the variable amends, we use eval here to first construct the command
  # by concatenating all the arguments together (space separated), then read and execute it
  echo "Pushing manifest for multi-arch image"
  eval "docker manifest create ${docker_image}:${base_version}${amends}"
  docker manifest push "${docker_image}:${base_version}"

  # Sign multi-arch manifest
  ./scripts/ci/docker_sign.sh "${docker_image}:${base_version}"
done

# Signature verification
for docker_image in ${docker_images}; do
  # Verify multi-arch manifest
  echo "### Verifying signature for multi-arch image: ${docker_image}:${base_version}"
  ./scripts/ci/docker_verify_signature.sh "${docker_image}:${base_version}"

  # Verify individual architecture images
  for docker_architecture in ${docker_architectures}; do
    target_arch_tag="${docker_architecture}_${base_version}"
    echo "### Verifying signature for architecture image: ${docker_image}:${target_arch_tag}"
    ./scripts/ci/docker_verify_signature.sh "${docker_image}:${target_arch_tag}"
  done
done

echo "### Successfully promoted images to tag '${base_version}'"
