#!/bin/sh
set -eu

DOCKERFILE=${1:?"Dockerfile missing"}

BINFMT_VERSION=9.2.2-52

LATEST_TAG="${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}"
LATEST_TAG_GENERIC="${CI_COMMIT_REF_SLUG}"

## Build multi-arch Docker image
gcp="${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/$DISTRIBUTION:$RELEASE-${LATEST_TAG}"
gcp_generic="${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/$DISTRIBUTION:$RELEASE-${LATEST_TAG_GENERIC}"
protected_gcp="${GCP_PROTECTED_REGISTRY}/tezos/tezos/$DISTRIBUTION:$RELEASE"

# Cross-platform emulator collection
# We cannot make this installation permanent inside the GitLab executor GCP image yet
# https://github.com/tonistiigi/binfmt/issues/75
docker run --privileged --rm "tonistiigi/binfmt:qemu-v${BINFMT_VERSION}" --install all

docker buildx create --use --name tezos || echo "Warning: Docker builder instance already exists"

echo "Building images to GCP registries ..."
# shellcheck disable=SC2046
docker buildx build --push \
  --platform linux/amd64,linux/arm64 \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  --cache-from="type=registry,mode=max,compression=zstd,ref=${gcp_generic}" \
  --cache-from="type=registry,mode=max,compression=zstd,ref=${protected_gcp}" \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --secret "id=npm_token,src=/tmp/npm_token.txt" \
  --build-arg IMAGE="$DISTRIBUTION:$RELEASE" \
  --build-arg APT_PROXY="${APT_PROXY_DEB:-}" \
  --build-arg NPM_REGISTRY_DOMAIN="${NPM_REGISTRY_DOMAIN:-}" \
  --build-arg NPM_REGISTRY="${NPM_REGISTRY:-}" \
  --tag "${gcp}" \
  --tag "${gcp_generic}" \
  $(if [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then echo "--tag=${protected_gcp}"; fi) \
  $(if [ "${DOCKER_FORCE_BUILD:-false}" = "true" ]; then echo "--no-cache"; fi) \
  -f "$DOCKERFILE" .
