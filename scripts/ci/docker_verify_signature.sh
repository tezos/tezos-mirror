#!/bin/sh
#
# Verify the signature of the releasing docker image(s) using Cosign
# Usage:
# - '$0 <IMAGE:TAG>' this script will verify the signature of the provided image reference
# - Without arguments, this script will verify all docker images listed in docker_images.
# Reads the following environment variables:
#  - 'GCP_SIGN_KEY_URL': key URL used to verify released Docker image signatures, set by GitLab CI

set -eu

# In every real pipeline GCP_SIGNER_SERVICE_ACCOUNT is set, so an empty value
# means a misconfiguration (the matching images would not have been signed).
# Fail with a clear error rather than crash on the unbound variable under
# [set -eu].
if [ -z "${GCP_SIGNER_SERVICE_ACCOUNT:-}" ]; then
  echo "GCP_SIGNER_SERVICE_ACCOUNT is not set; cannot verify image signatures." >&2
  exit 1
fi

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

# Read environment variables written by 'docker_image_names.sh'
# (via docker_initialize.sh --image-names) in 'before_script'.
. scripts/ci/docker.env

# Install cosign
apk add --update cosign
cosign version

# Retry [imagetools inspect]: a transient registry error returns an empty
# digest, which makes cosign fail with "invalid reference format".
MAX_RETRY_ATTEMPTS=5
SLEEP_TIME=5

# Usage: verify_image <IMAGE> <TAG>
verify_image() {
  # Do not pull: per-arch tags are OCI indexes, so pulling one whose platform
  # does not match the host fails with "no matching manifest". [imagetools
  # inspect] resolves the index digest remotely.
  retry_attempt=0
  IMAGE_DIGEST=''
  until [ "${retry_attempt}" -ge "${MAX_RETRY_ATTEMPTS}" ]; do
    _digest=$(docker buildx imagetools inspect "$1:$2" --format "{{json .Manifest}}" | jq -r '.digest') || _digest=''
    if [ -n "${_digest}" ] && [ "${_digest}" != "null" ]; then
      IMAGE_DIGEST="$1@${_digest}"
      break
    fi
    retry_attempt="$((retry_attempt + 1))"
    echo "Empty digest for $1:$2, retrying in ${SLEEP_TIME}s..."
    sleep "${SLEEP_TIME}"
  done
  if [ -z "${IMAGE_DIGEST}" ]; then
    echo "Fatal: Failed to retrieve digest for $1:$2 after ${MAX_RETRY_ATTEMPTS} attempts."
    exit 1
  fi
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
