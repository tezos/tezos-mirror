#!/bin/sh
set -eu

### Promote a packaging revision image to version tag (remove revision suffix)
### e.g., octez-v24.0-1 -> octez-v24.0

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Get all tags pointing at HEAD and filter for packaging revision format (octez-vX.Y-N)
# Sort by version and take the latest packaging revision
ci_commit_tag=$(git tag --points-at HEAD | grep -E '^octez-v[0-9]+\.[0-9]+-[0-9]+$' | sort -V | tail -1)

if [ -z "${ci_commit_tag}" ]; then
  echo "Error: No packaging revision tag (octez-vX.Y-N format) found at HEAD"
  echo "Available tags at HEAD:"
  git tag --points-at HEAD
  exit 1
fi

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

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Promoting docker image: ${docker_image}"

  # First, promote individual architecture images
  for docker_architecture in ${docker_architectures}; do
    source_tag="${docker_architecture}_${CI_COMMIT_TAG}"
    target_arch_tag="${docker_architecture}_${base_version}"

    echo "Pulling ${docker_image}:${source_tag}"
    docker pull "${docker_image}:${source_tag}"

    # Verify signature before promoting
    ./scripts/ci/docker_verify_signature.sh "${docker_image}:${source_tag}"

    echo "Tagging ${docker_image}:${source_tag} -> ${docker_image}:${target_arch_tag}"
    docker tag "${docker_image}:${source_tag}" "${docker_image}:${target_arch_tag}"
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
  target_tag="${base_version}"
  eval "docker manifest create ${docker_image}:${target_tag}${amends}"
  docker manifest push "${docker_image}:${target_tag}"

  # Sign multi-arch manifest
  ./scripts/ci/docker_sign.sh "${docker_image}:${target_tag}"
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
