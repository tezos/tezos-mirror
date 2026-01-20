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

# Native vs emulated build.
# if we set the platform to only one architecture, we
# add an additional information to the image label.
# if we specify a platform for both, we install qemu
# If we specify a $PLATFORM on the wrong host arch, we'll get an error
case "${PLATFORM:-}" in
"linux/amd64")
  # In case we only want to compile for one architecture.
  # This is a native compilation as the default runner is amd64
  : nop
  ;;
"linux/amd64,linux/arm64")
  # Cross-platform emulator collection
  # We cannot make this installation permanent inside the GitLab
  # executor GCP image yet
  # https://github.com/tonistiigi/binfmt/issues/75
  # we assume we always compile using amd64 host and emulate arm64
  docker run --privileged --rm "tonistiigi/binfmt:qemu-v${BINFMT_VERSION}" --install "arm64"
  ;;
*)
  # Determine platform based on runner's tags
  case "$TAGS" in
  gcp_arm64 | gcp_dev_arm64)
    PLATFORM="linux/arm64"
    ARCHITECTURE="arm64"
    ;;
  gcp | gcp_dev | gcp_high_cpu | gcp_high_cpu_dev | gcp_very_high_cpu | gcp_very_high_cpu_dev | gcp_very_high_cpu_ramfs | gcp_very_high_cpu_ramfs_dev)
    PLATFORM="linux/amd64"
    ARCHITECTURE="amd64"
    ;;
  *)
    echo "Unknown tag: $TAGS"
    exit 1
    ;;
  esac
  gcp="${gcp}-${ARCHITECTURE}"
  gcp_generic="${gcp_generic}-${ARCHITECTURE}"
  protected_gcp="${protected_gcp}-${ARCHITECTURE}"
  ;;
esac

docker buildx create --driver docker-container --use --name tezos || echo "Warning: Docker builder instance already exists"

echo "Building images to GCP registries ..."
echo "IMAGE=$IMAGE_PATH:$RELEASE"
echo "gcp=${gcp}"
echo "gcp_generic=${gcp_generic}"
echo "PLATFORM=$PLATFORM"

set -x

# shellcheck disable=SC2046
docker buildx build --push \
  --platform "$PLATFORM" \
  --provenance=false \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  --cache-from="type=registry,ref=${gcp_generic}-cache" \
  --cache-from="type=registry,ref=${protected_gcp}" \
  --cache-to="type=registry,ref=${gcp_generic}-cache" \
  --secret "id=npm_token,src=/tmp/npm_token.txt" \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --build-arg IMAGE="$IMAGE_PATH:$RELEASE" \
  --build-arg APT_PROXY="${APT_PROXY_DEB:-}" \
  --build-arg NPM_REGISTRY_DOMAIN="${NPM_REGISTRY_DOMAIN:-}" \
  --build-arg NPM_REGISTRY="${NPM_REGISTRY:-}" \
  --tag "${gcp}" \
  --tag "${gcp_generic}" \
  $(if [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then echo "--tag=${protected_gcp}"; fi) \
  $(if [ "${DOCKER_FORCE_BUILD:-false}" = "true" ]; then echo "--no-cache"; fi) \
  -f "$DOCKERFILE" .
