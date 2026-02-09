#!/bin/sh
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Setup Docker names such that they are valid for the target registry.
# This script write the docker.env file containing the prefix to use
# while publishing images either on out registry or on dockerhub

# Docker constraints on tags:
# https://docs.docker.com/engine/reference/commandline/tag/

# a simple library of functions used in multiple scripts
# shellcheck source=scripts/ci/docker_registry.inc.sh
. "$current_dir"/docker_registry.inc.sh

echo '### Input variables'
echo "CI_COMMIT_REF_NAME=${CI_COMMIT_REF_NAME}"
echo "CI_DOCKER_HUB=${CI_DOCKER_HUB:-}"
echo "CI_PROJECT_NAME=${CI_PROJECT_NAME}"
echo "CI_PROJECT_NAMESPACE=${CI_PROJECT_NAMESPACE}"
echo "IMAGE_ARCH_PREFIX=${IMAGE_ARCH_PREFIX:-}"
echo "DOCKER_BUILD_TARGET=${DOCKER_BUILD_TARGET:-}"
echo "RUST_TOOLCHAIN_IMAGE_NAME=${RUST_TOOLCHAIN_IMAGE_NAME:-}"
echo "CI_COMMIT_REF_PROTECTED=${CI_COMMIT_REF_PROTECTED:-}"

if [ "${CI_DOCKER_HUB:-}" = 'true' ] && [ "${CI_PROJECT_NAMESPACE}" = "tezos" ] && [ -n "${CI_DOCKER_AUTH:-}" ]; then
  docker_image_name="docker.io/${CI_PROJECT_PATH}-"
else
  # default docker image name prefix. For example it is used to prefix the images
  # tezos/tezos , tezos/tezos-bare, tezos/tezos-debug
  # The GCP_RELEASE_REGISTRY has a different value for protected and not
  # protected branches and it's configured in the gitlab CI
  docker_image_name="${GCP_RELEASE_REGISTRY}/${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}/"
fi

docker_image_tag=$(echo "${IMAGE_ARCH_PREFIX:-}${CI_COMMIT_REF_NAME}" | sanitizeTag)

# Write computed Docker environment variables to
# sourceable file for other shell scripts
# these are used in the
# - directly docker_release.sh
# - all scripts sourcing docker.sh and docker.env
# define debug, bare, and minimal for user images and build for the ci

echo "export DOCKER_IMAGE_NAME=${docker_image_name}" > "${current_dir}/docker.env"
echo "export DOCKER_IMAGE_TAG=${docker_image_tag}" >> "${current_dir}/docker.env"

# Print out the results to verify everything is ok:

# shellcheck source=./scripts/ci/docker.env
. "${current_dir}/docker.env"
# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

echo '### Docker image names:'

echo "${docker_build_image}:${DOCKER_IMAGE_TAG}"

for docker_image in ${docker_images}; do
  echo "${docker_image}:${DOCKER_IMAGE_TAG}"
done
