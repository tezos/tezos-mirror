#!/bin/bash
set -euo pipefail
set -x
# Bash fork from Kaniko GitLab template
# https://gitlab.com/gitlab-org/gitlab/-/blob/master/lib/gitlab/ci/templates/Kaniko.gitlab-ci.yml

. ./images_base/ci-docker/scripts/tag.sh

### Log in to Gitlab and GCP Artifact container registries
./images_base/ci-docker/scripts/docker_registry_auth.sh

## Build multi-arch Docker image
gitlab="${CONTAINER_NAME}:${DOCKER_IMAGE_TAG}"
gcp="${GCP_ARTIFACT_REGISTRY_IMAGE}:${DOCKER_IMAGE_TAG}"

# Cross-platform emulator collection
# We cannot make this installation permanent inside the GitLab executor GCP image yet
# https://github.com/tonistiigi/binfmt/issues/75
docker run --privileged --rm "tonistiigi/binfmt:qemu-v${BINFMT_VERSION}" --install all

docker buildx create --use --name tezos || echo "Warning: Docker builder instance already exists"

# Build and push to GitLab container registry and GCP Artifact Registry
if [ "${CI_COMMIT_REF_PROTECTED:-false}" = false ]; then
  echo "Building docker images to Gitlab and GCP registries."
  docker buildx build --push \
    --platform linux/amd64,linux/arm64 \
    --tag "${gitlab}" \
    --tag "${gcp}" \
    --build-arg DOCKER_VERSION=${DOCKER_VERSION} \
    -f ./images_base/ci-docker/Dockerfile .
else
  # In the case where we run on a protected branch is available we push the image
  # also to the protected GCP Artifact Resgitry.
  echo "Building images to Gitlab and GCP registries including protected repository ..."
  protected_gcp="${GCP_PROTECTED_ARTIFACT_REGISTRY_IMAGE}:${DOCKER_IMAGE_TAG}"
  docker buildx build --push \
    --platform linux/amd64,linux/arm64 \
    --tag "${gitlab}" \
    --tag "${gcp}" \
    --tag "${protected_gcp}" \
    --build-arg DOCKER_VERSION=${DOCKER_VERSION} \
    -f ./images_base/ci-docker/Dockerfile .
fi
