#!/bin/sh

# create_and_push_manifest TAG
#
# This function creates and pushes a multi-architecture Docker manifest for the
# specified tag. It references two platform-specific images (amd64 and arm64)
# that were previously pushed using a unique build tag derived from the current
# Git commit.
#
# Arguments:
#   TAG - The tag to apply to the multi-arch manifest (e.g., "main", "develop",
#   or a commit-based tag).
#
# Behavior:
#   - Assembles a manifest list for the given tag using architecture-specific
#     image tags: ${LATEST_TAG}-amd64 and ${LATEST_TAG}-arm64
#   - Annotates each image with its respective architecture
#   - Pushes the final multi-arch manifest to the container registry
#
# Usage:
#   create_and_push_manifest "${LATEST_TAG}"
#   create_and_push_manifest "${LATEST_TAG_GENERIC}"
#
# Note:
#   Assumes IMAGE_NAME, LATEST_TAG, and platform-specific images are already
#   set and pushed.

LATEST_TAG="${RELEASE}-${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}"
LATEST_TAG_GENERIC="${RELEASE}-${CI_COMMIT_REF_SLUG}"

create_and_push_manifest() {
  TAG="$1"

  echo "Inspect ${IMAGE_NAME}:${LATEST_TAG}-amd64"
  docker buildx imagetools inspect "${IMAGE_NAME}:${LATEST_TAG}-amd64"

  set +e # Disable exit on error temporarily

  echo "Inspect ${IMAGE_NAME}:${LATEST_TAG}-arm64"
  docker buildx imagetools inspect "${IMAGE_NAME}:${LATEST_TAG}-arm64"
  ARM64_INSPECT=$?

  set -e # Re-enable exit on error

  echo "📦 Creating multi-arch manifest for tag: ${TAG}"

  echo "Adding ${IMAGE_NAME}:${LATEST_TAG}-amd64 to the manifest."
  AMEND="--amend ${IMAGE_NAME}:${LATEST_TAG}-amd64"

  # Amend the manifest if the arm64 image exists
  if [ $ARM64_INSPECT -eq 0 ]; then
    echo "Adding ${IMAGE_NAME}:${LATEST_TAG}-arm64 to the manifest."
    AMEND="$AMEND --amend ${IMAGE_NAME}:${LATEST_TAG}-arm64"
  fi

  #shellcheck disable=SC2086
  docker manifest create "${IMAGE_NAME}:${TAG}" ${AMEND}

  # Push the manifest
  docker manifest push "${IMAGE_NAME}:${TAG}"

  echo "Pushed manifest: ${IMAGE_NAME}:${TAG}"

}

create_and_push_manifest "${LATEST_TAG}"
create_and_push_manifest "${LATEST_TAG_GENERIC}"
if [ "${CI_COMMIT_REF_PROTECTED:-}" = "true" ]; then
  create_and_push_manifest "${RELEASE}"
fi
