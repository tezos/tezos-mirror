#!/bin/sh

# Build dependency images for debian based distributions

set -e

. scripts/version.sh

# Determine platform based on runner's tags
# This works with GCP. If switching to AWS these tags must be changed.
case "$TAGS" in
gcp_arm64)
  PLATFORM="linux/arm64"
  ARCHITECTURE="arm64"
  ;;
gcp)
  PLATFORM="linux/amd64"
  ARCHITECTURE="amd64"
  ;;
*)
  echo "Unknown tag: $TAGS"
  exit 1
  ;;
esac

docker build \
  --network host \
  --platform $PLATFORM \
  --push \
  --label "com.tezos.build-pipeline-id"="${CI_PIPELINE_ID}" \
  --label "com.tezos.build-pipeline-url"="${CI_PIPELINE_URL}" \
  --label "com.tezos.build-job-id"="${CI_JOB_ID}" \
  --label "com.tezos.build-job-url"="${CI_JOB_URL}" \
  --label "com.tezos.build-tezos-revision"="${CI_COMMIT_SHA}" \
  --label "com.tezos.build-opam_repository_tag"="${opam_repository_tag}" \
  --cache-from="${DEP_IMAGE}:${ARCHITECTURE}" \
  -f debian-deps-build.Dockerfile \
  --build-arg=BUILDKIT_INLINE_CACHE=1 \
  --build-arg IMAGE="$DISTRIBUTION:$RELEASE" \
  -t "$DEP_IMAGE:${ARCHITECTURE}" \
  -t "$DEP_IMAGE:latest" \
  .

docker buildx imagetools create --append \
  -t "${DEP_IMAGE}:latest" "${DEP_IMAGE}:${ARCHITECTURE}"

docker buildx imagetools inspect "${DEP_IMAGE}:latest"
