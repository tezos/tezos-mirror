#!/bin/sh
#
# Verify the signature of the releasing docker image(s) using Cosign
# Usage:
# - '$0 <IMAGE:TAG>' this script will verify the signature of the provided image reference
# - Without arguments, this script will verify all docker images listed in docker_images.
# Reads the following environment variables:
#  - 'GCP_SIGN_KEY_URL': key URL used to verify released Docker image signatures, set by GitLab CI

set -eu

echo "Starting Docker image signature verification process..."

# Check if argument is valid
if [ $# -gt 1 ]; then
  echo "Usage: $0 [<IMAGE:TAG>]"
  exit 1
elif [ $# -eq 1 ]; then
  # Extract IMAGE and TAG from argument when provided
  IMAGE="${1%:*}"
  TAG="${1##*:}"
  # Check the result
  if [ "$1" = "${IMAGE}" ] || [ -z "${IMAGE}" ] || [ -z "${TAG}" ]; then
    echo "Format error: When needed, the first optional argument must respect <IMAGE:TAG> format"
    echo "Usage: $0 [<IMAGE:TAG>]"
    exit 1
  fi
fi

# Read environment variables written by 'docker_registry_auth.sh' in
# 'before_script'.
. scripts/ci/docker.env

# Install cosign
apk add --update cosign
cosign version

# Usage: verify_image <IMAGE> <TAG>
verify_image() {
  # Pull images
  docker pull "$1:$2"

  # Get image digest (better than tag for precision)
  IMAGE_DIGEST="$(docker image inspect "$1:$2" --format="{{index .RepoDigests 0}}")"
  echo "Image digest to verify: ${IMAGE_DIGEST}"

  # Get the location of image signature as reference
  IMAGE_SIGNATURE_LOCATION=$(cosign triangulate "${IMAGE_DIGEST}")
  echo "Image signature location: ${IMAGE_SIGNATURE_LOCATION}"

  # Verify the signature
  cosign verify --key publickey.pem "${IMAGE_DIGEST}" | jq
}

# Get public key
wget -O publickey.pem "${GCP_SIGN_KEY_URL}"
cat publickey.pem

# Verify the provided image or verify all images provided in $docker_images
if [ $# -eq 1 ]; then
  verify_image "${IMAGE}" "${TAG}"
else
  # Get context
  current_dir=$(cd "$(dirname "${0}")" && pwd)

  # shellcheck source=./scripts/ci/docker.sh
  . "${current_dir}/docker.sh"

  # Loop over images in $docker_images and use $DOCKER_IMAGE_TAG as tag
  for docker_image in ${docker_images}; do
    verify_image "${docker_image}" "${DOCKER_IMAGE_TAG}"
  done
fi

echo "Docker image signature verification process done."
