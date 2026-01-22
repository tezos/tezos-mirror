#!/bin/sh

# Build dependency images for debian based distributions

set -e

DOCKERFILE=${1:-images/packages/debian-deps-build.Dockerfile}

. scripts/version.sh

# Determine platform based on runner's tags
# This works with GCP. If switching to AWS these tags must be changed.
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

ARCH_IMAGE_PREFIX="${DEP_IMAGE}"
ARCH_IMAGE_TAG="${RELEASE}-${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}-${ARCHITECTURE}"
ARCH_IMAGE_NAME="${ARCH_IMAGE_PREFIX}:${ARCH_IMAGE_TAG}"

# Set as a variable in the job definition
BASE_IMAGE="${BASE_IMAGE:?Must set base image to run this script}"

# Enforce image rebuild ignoring inputs changes (i.e. rebuild the image
# for security updates purposes)
skip_registry_cache_check=${DOCKER_FORCE_BUILD:-"false"}
if [ "$skip_registry_cache_check" = "true" ]; then
  echo "Force rebuild of CI images, using no cached layers"
fi

# shellcheck disable=SC2046
docker build \
  --network host \
  --platform $PLATFORM \
  --push \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  -f "$DOCKERFILE" \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --build-arg IMAGE="${BASE_IMAGE}" \
  --build-arg OPAM_VERSION="${opam_version}" \
  --cache-from="${DEP_IMAGE}:${ARCHITECTURE}-${CI_COMMIT_REF_SLUG}" \
  --cache-from="${DEP_IMAGE_PROTECTED}:master" \
  -t "$ARCH_IMAGE_NAME" \
  $(if [ "$skip_registry_cache_check" = "true" ]; then echo "--no-cache"; fi) \
  .
