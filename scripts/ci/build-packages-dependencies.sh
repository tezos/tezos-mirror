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

LOCAL_IMAGE_NAME="$DEP_IMAGE:${ARCHITECTURE}-${CI_COMMIT_REF_SLUG}"
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
  -t "$LOCAL_IMAGE_NAME" \
  $(if [ "$skip_registry_cache_check" = "true" ]; then echo "--no-cache"; fi) \
  .

# The LATEST_TAG is used to compile the images in the CI pipelines
# where we want to make sure to always have the latest available
# image ( we use IfNotPresent as pull policy )
# The LATEST_TAG_GENERIC tag is used for the docker cache and
# general use where we can control the pull policy
LATEST_TAG="${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}"
LATEST_TAG_GENERIC="${CI_COMMIT_REF_SLUG}"

if [ "$skip_registry_cache_check" != "true" ]; then
  echo "Checking for existence of image $DEP_IMAGE:$LATEST_TAG"
  docker buildx imagetools inspect "$DEP_IMAGE:$LATEST_TAG" || export IMAGE_EXISTS="false"
else
  export IMAGE_EXISTS="false"
  echo "Force rebuild of CI images: we use the locally build image for the manifest"
fi

if [ "$IMAGE_EXISTS" = "false" ]; then
  echo "Creating image manifesto for $DEP_IMAGE:$LATEST_TAG"
  docker buildx imagetools create -t "$DEP_IMAGE:$LATEST_TAG" "$LOCAL_IMAGE_NAME"
else
  echo "Appending to image manifesto for $DEP_IMAGE:$LATEST_TAG with new ${ARCHITECTURE} image"

  # we get the sha of the image of the "other" architecture
  OTHER_SHA=$(docker manifest inspect "${DEP_IMAGE}:$LATEST_TAG" | jq -r ".manifests[] | select(.platform.architecture != \"$ARCHITECTURE\") | .digest" | head -n1)

  if [ "${OTHER_SHA}" != "" ]; then
    # we create a new manifest ( override the old one ) with both the
    # local image and the image that was in the old manifest
    docker buildx imagetools create \
      -t "${DEP_IMAGE}:$LATEST_TAG" \
      -t "${DEP_IMAGE}:$LATEST_TAG_GENERIC" \
      "$LOCAL_IMAGE_NAME" \
      "$DEP_IMAGE@$OTHER_SHA"
  else
    # we associate to "$DEP_IMAGE:$LATEST_TAG" the latest image with tag
    # "$LOCAL_IMAGE_NAME" and replace the old one.
    docker buildx imagetools create \
      -t "$DEP_IMAGE:$LATEST_TAG" \
      -t "$DEP_IMAGE:$LATEST_TAG_GENERIC" \
      "$LOCAL_IMAGE_NAME"
  fi
fi

docker buildx imagetools inspect "${DEP_IMAGE}:${LATEST_TAG}"
