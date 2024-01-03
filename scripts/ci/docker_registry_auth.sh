#!/bin/sh
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Setup Docker registry authentication for either Docker Hub (release) or GitLab registry (dev).
# Also setup Docker names such that they are valid for the target (Docker Hub or GitLab).
# Docker constraints on tags:
# https://docs.docker.com/engine/reference/commandline/tag/

#  A tag name must be valid ASCII and may contain lowercase and
#  uppercase letters, digits, underscores, periods and dashes. A tag
#  name may not start with a period or a dash and may contain a maximum
#  of 128 characters.

# GitLab constraints on images:
# https://docs.gitlab.com/ee/user/packages/container_registry/#image-naming-convention

# /!\ CI_REGISTRY is overriden to use AWS ECR in `nomadic-labs` and `tezos` GitLab namespaces

# Create directory for Docker JSON configuration (if does not exist)
mkdir -pv ~/.docker

echo '### Input variables'
echo "CI_COMMIT_REF_NAME=${CI_COMMIT_REF_NAME}"
echo "CI_DOCKER_HUB=${CI_DOCKER_HUB:-}"
echo "CI_PROJECT_NAME=${CI_PROJECT_NAME}"
echo "CI_PROJECT_NAMESPACE=${CI_PROJECT_NAMESPACE}"
echo "IMAGE_ARCH_PREFIX=${IMAGE_ARCH_PREFIX:-}"
echo "DOCKER_BUILD_TARGET=${DOCKER_BUILD_TARGET:-}"
echo "RUST_TOOLCHAIN_IMAGE=${RUST_TOOLCHAIN_IMAGE:-}"

# CI_DOCKER_HUB is used to switch to Docker Hub if credentials are available with CI_DOCKER_AUTH
# /!\ CI_DOCKER_HUB can be unset, CI_DOCKER_AUTH is only available on protected branches
if [ "${CI_DOCKER_HUB:-}" = 'true' ] && [ "${CI_PROJECT_NAMESPACE}" = "tezos" ] && [ -n "${CI_DOCKER_AUTH:-}" ]; then
  # Docker Hub
  echo "### Logging into Docker Hub for pushing images"
  docker_image_name="docker.io/${CI_PROJECT_PATH}-"
  echo "{\"auths\":{\"https://index.docker.io/v1/\":{\"auth\":\"${CI_DOCKER_AUTH}\"}}}" > ~/.docker/config.json
else
  # GitLab container registry
  echo "### Logging into Gitlab Container Registry for pushing images"
  docker login -u "${CI_REGISTRY_USER}" -p "${CI_REGISTRY_PASSWORD}" registry.gitlab.com
  docker_image_name="registry.gitlab.com/${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}/"
fi

# Allow to pull from private AWS ECR if used as CI_REGISTRY
if echo "${CI_REGISTRY}" | grep -q '\.dkr\.ecr\.'; then
  echo "### Logging into Amazon ECR for pulling images"
  if [ ! -f "/secrets/.aws_ecr/CI_AWS_ECR_TOKEN" ]; then
    echo "Use Amazon ECR Docker Credential Helper"
    # Make sure Amazon ECR Docker Credential Helper is installed
    docker-credential-ecr-login version > /dev/null
    # Merge with existing Docker client configuration
    jq ". + {\"credHelpers\": { \"${CI_REGISTRY}\": \"ecr-login\"}}" ~/.docker/config.json | sponge ~/.docker/config.json
  else
    echo "Use the stored ECR token"
    docker login --username AWS --password-stdin "${CI_REGISTRY}" < /secrets/.aws_ecr/CI_AWS_ECR_TOKEN
  fi
  echo "### Amazon ECR Docker Credential Helper enabled for ${CI_REGISTRY}"
fi

# /!\ IMAGE_ARCH_PREFIX can be unset
docker_image_tag=$(echo "${IMAGE_ARCH_PREFIX:-}${CI_COMMIT_REF_NAME}" | tr -c -- '-._\n[:alnum:]' '_')

## Write computed Docker environment variables to sourceable file for other shell scripts

echo "export DOCKER_IMAGE_NAME=${docker_image_name}" > "${current_dir}/docker.env"
echo "export DOCKER_IMAGE_TAG=${docker_image_tag}" >> "${current_dir}/docker.env"

# shellcheck source=./scripts/ci/docker.env
. "${current_dir}/docker.env"
# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

echo '### Docker image names:'

echo "${docker_build_image}:${DOCKER_IMAGE_TAG}"

for docker_image in ${docker_images}; do
  echo "${docker_image}:${DOCKER_IMAGE_TAG}"
done
