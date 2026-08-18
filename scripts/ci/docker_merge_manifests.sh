#!/bin/sh
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

short_tag="${DOCKER_IMAGE_TAG}"

last_commit_date_time=$(git log --pretty=format:"%cd" -1 --date="format:%Y%m%d%H%M%S" 2>&1)
# Format: tag_1234abcd_YYYYMMDDHHMMSS
long_tag="${short_tag}_${CI_COMMIT_SHORT_SHA}_${last_commit_date_time}"

docker_tags="${short_tag} ${long_tag}"

# Loop over images
for docker_image in ${docker_images}; do
  echo "### Merging tags for docker image: ${docker_image}"

  sources=''
  for docker_architecture in ${docker_architectures}; do
    sources="${sources} ${docker_image}:${docker_architecture}_${short_tag}"
  done

  # Loop over tags
  for docker_tag in ${docker_tags}; do
    # [imagetools create] preserves the attestation manifests (SLSA
    # provenance, SBOM) attached to the per-arch images as OCI referrers.
    # shellcheck disable=SC2086
    docker buildx imagetools create --tag "${docker_image}:${docker_tag}" ${sources}

    # Sign image
    ./scripts/ci/docker_sign.sh "${docker_image}:${docker_tag}"
  done
done

# Signature verification
for docker_image in ${docker_images}; do
  echo "### Verifying signature for docker image: ${docker_image}"
  for docker_tag in ${docker_tags}; do
    ./scripts/ci/docker_verify_signature.sh "${docker_image}:${docker_tag}"
  done
done

# Attestation verification: the SLSA provenance and SBOM referrers must still
# be reachable on the merged multi-arch tags.
for docker_image in ${docker_images}; do
  echo "### Verifying attestations for docker image: ${docker_image}"
  for docker_tag in ${docker_tags}; do
    ./scripts/ci/docker_verify_attestation.sh "${docker_image}:${docker_tag}"
  done
done
