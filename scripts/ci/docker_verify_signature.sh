#!/bin/sh
#
# Verify the signature of the releasing docker images using Cosign
#
# Reads the following environment variables:
#  - 'GCP_SIGN_KEY_URL': key URL used to verify released Docker image signatures, set by GitLab CI

set -eu

# Read environment variables written by 'docker_registry_auth.sh' in
# 'before_script'.
. scripts/ci/docker.env

current_dir=$(cd "$(dirname "${0}")" && pwd)

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

# Install cosign
apk add --update cosign
cosign version

# Get public key
wget -O publickey.pem "${GCP_SIGN_KEY_URL}"
cat publickey.pem

# Loop over images
for docker_image in ${docker_images}; do

  # Pull images
  docker pull "${docker_image}:${DOCKER_IMAGE_TAG}"

  # Get image digest
  IMAGE_DIGEST="$(docker image inspect "${docker_image}:${DOCKER_IMAGE_TAG}" --format="{{index .RepoDigests 0}}")"
  echo "Image digest: ${IMAGE_DIGEST}"

  # Get the location of image signature as reference
  IMAGE_SIGNATURE_LOCATION=$(cosign triangulate "${IMAGE_DIGEST}")
  echo "Image signature location: ${IMAGE_SIGNATURE_LOCATION}"

  # Verify the signature
  cosign verify --key publickey.pem "${IMAGE_DIGEST}" | jq
done
