#!/bin/bash
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Setup Docker registry authentication for either Docker Hub (release) or GCP Artefact registry (dev).
# Also setup Docker names such that they are valid for the target registry.
# Docker constraints on tags:
# https://docs.docker.com/engine/reference/commandline/tag/

#  A tag name must be valid ASCII and may contain lowercase and
#  uppercase letters, digits, underscores, periods and dashes. A tag
#  name may not start with a period or a dash and may contain a maximum
#  of 128 characters.

logged_in=false

# Create directory for Docker JSON configuration (if does not exist)
mkdir -pv ~/.docker

echo '### Input variables'
echo "CI_COMMIT_REF_NAME=${CI_COMMIT_REF_NAME}"
echo "CI_DOCKER_HUB=${CI_DOCKER_HUB:-}"
echo "CI_PROJECT_NAME=${CI_PROJECT_NAME}"
echo "CI_PROJECT_NAMESPACE=${CI_PROJECT_NAMESPACE}"
echo "IMAGE_ARCH_PREFIX=${IMAGE_ARCH_PREFIX:-}"
echo "DOCKER_BUILD_TARGET=${DOCKER_BUILD_TARGET:-}"
echo "RUST_TOOLCHAIN_IMAGE_NAME=${RUST_TOOLCHAIN_IMAGE_NAME:-}"
echo "CI_COMMIT_REF_PROTECTED=${CI_COMMIT_REF_PROTECTED:-}"

# Export Google Auth token to build DockerFile
echo "Current active user:"
gcloud config get-value account
echo -n "$(gcloud auth print-access-token)" > /tmp/npm_token.txt

# Allow to push to private GCP Artifact Registry if the CI/CD variable is defined
if [ -n "${GCP_REGISTRY:-}" ]; then
  # There are two registries for storing Docker images. The first allows pushes from
  # Tezos CI jobs on unprotected branches. The second is accessible for push
  # operation only from protected branches for security reasons. Finally, both
  # registries are publicly accessible for pulls.
  if [ "${CI_COMMIT_REF_PROTECTED:-false}" = true ]; then
    echo "### Logging into protected GCP Artifact Registry for pushing images"
    echo "${GCP_PROTECTED_SERVICE_ACCOUNT}" | base64 -d > protected_sa.json
    gcloud auth activate-service-account --key-file=protected_sa.json
    gcloud auth configure-docker us.gcr.io
    gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin https://us-central1-docker.pkg.dev
    rm protected_sa.json
  else
    echo "### Logging into standard GCP Artifact Registry for pushing images"
    GCP_ARTIFACT_REGISTRY_TOKEN=$(curl -s -H "Metadata-Flavor: Google" http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token | cut -d'"' -f4)
    echo "${GCP_ARTIFACT_REGISTRY_TOKEN}" | docker login us-central1-docker.pkg.dev -u oauth2accesstoken --password-stdin
  fi
  docker_image_name="${GCP_REGISTRY}/${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}/"
  logged_in=true
fi

# CI_DOCKER_HUB is used to switch to Docker Hub if credentials are available with CI_DOCKER_AUTH
# /!\ CI_DOCKER_HUB can be unset, CI_DOCKER_AUTH is only available on protected branches
if [ "${CI_DOCKER_HUB:-}" = 'true' ] && [ "${CI_PROJECT_NAMESPACE}" = "tezos" ] && [ -n "${CI_DOCKER_AUTH:-}" ]; then
  # Docker Hub
  echo "### Logging into Docker Hub for pushing images"
  # Over-write docker_image_name if set.
  # Publishing to Docker Hub, as done on protected refs, has higher priority.
  docker_image_name="docker.io/${CI_PROJECT_PATH}-"
  echo "{\"auths\":{\"https://index.docker.io/v1/\":{\"auth\":\"${CI_DOCKER_AUTH}\"}}}" > ~/.docker/config.json
  logged_in=true
fi

if [ "$logged_in" = false ]; then
  echo "WARNING: No login to a docker registry was done during the call of this script."
fi

# shellcheck source=scripts/ci/docker_registry.inc.sh
. "$current_dir"/docker_registry.inc.sh

# /!\ IMAGE_ARCH_PREFIX can be unset
docker_image_tag=$(echo "${IMAGE_ARCH_PREFIX:-}${CI_COMMIT_REF_NAME}" | sanitizeTag)

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
