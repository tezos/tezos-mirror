#!/bin/sh
#
# Sign the releasing docker image(s) using Cosign.
# Usage:
# - '$0 <IMAGE:TAG>' this script will sign the provided image reference
# and its layers or its sub-images in case of multi-arch images
# - Without arguments, this script will sign all docker images listed in docker_images.
#
# Reads the following environment variables:
#  - 'GCP_SIGNER_SERVICE_ACCOUNT': signer service account key file encoded
#     in base64, set by GitLab CI
#  - 'GCP_SIGN_KEY': key used for signing, format:
#     `gcpkms://projects/<GCP_PROJECT_CONTAINING_THE_KEY_PAIR>/locations/<LOCATION>/keyRings/<KEYRING_NAME>/cryptoKeys/<KEY_NAME>/versions/<KEY_VERSION>`,
#     set by GitLab CI

set -eu

echo "Starting Docker image signing process..."

# In case Cosign crashes during signature process especially during TLog upload
# (i.e Rekor Transparency Log timeout)
MAX_RETRY_ATTEMPTS=5
SLEEP_TIME=5

# Check if argument is valid
if [ $# -gt 1 ]; then
  echo "Usage: $0 [<IMAGE:TAG>]"
  exit 1
fi
if [ $# -eq 1 ]; then
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

# Auth signer service account
echo "${GCP_SIGNER_SERVICE_ACCOUNT}" | base64 -d > signer_sa.json
gcloud auth activate-service-account --key-file=signer_sa.json
gcloud auth configure-docker us-central1-docker.pkg.dev
gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin https://us-central1-docker.pkg.dev
export GOOGLE_APPLICATION_CREDENTIALS=signer_sa.json

# Install cosign
apk add --update cosign
cosign version

# Usage: sign_image <IMAGE> <TAG> [recursive]
sign_image() {
  # Get image digest (better than tag for precision)
  IMAGE_DIGEST="$1@$(docker buildx imagetools inspect "$1:$2" --format "{{json .Manifest}}" | jq -r '.digest')"
  echo "Image digest to sign: ${IMAGE_DIGEST}"

  # Disable error catching to be able to retry
  set +e

  # Sign image with cosign
  # Check if the image is a multi-arch image
  # --recursive is needed to sign all included images and not only the manifest
  # list for multi-arch images

  retry_attempt=0
  if [ "$(docker manifest inspect "${IMAGE_DIGEST}" | jq '.manifests | length > 1')" ]; then
    until [ "${retry_attempt}" -ge "${MAX_RETRY_ATTEMPTS}" ]; do
      cosign sign --key "${GCP_SIGN_KEY}" "${IMAGE_DIGEST}" --recursive -y && break
      retry_attempt="$((retry_attempt + 1))"
      if [ "${retry_attempt}" -ge "${MAX_RETRY_ATTEMPTS}" ]; then
        echo "Fatal: Failed to sign recursively ${IMAGE_DIGEST} after ${MAX_RETRY_ATTEMPTS} attempts."
        exit 1
      else
        echo "Recursive signing failed, retrying in ${SLEEP_TIME}s..."
        sleep "${SLEEP_TIME}"
      fi
    done
  else
    until [ "${retry_attempt}" -ge "${MAX_RETRY_ATTEMPTS}" ]; do
      cosign sign --key "${GCP_SIGN_KEY}" "${IMAGE_DIGEST}" -y && break
      retry_attempt="$((retry_attempt + 1))"
      if [ "${retry_attempt}" -ge "${MAX_RETRY_ATTEMPTS}" ]; then
        echo "Fatal: Failed to sign ${IMAGE_DIGEST} after ${MAX_RETRY_ATTEMPTS} attempts."
        exit 1
      else
        echo "Signing failed, retrying in ${SLEEP_TIME}s..."
        sleep "${SLEEP_TIME}"
      fi
    done
  fi
  set -e

  # Get the location of image signature as reference
  IMAGE_SIGNATURE_LOCATION=$(cosign triangulate "${IMAGE_DIGEST}")
  echo "Image signature location: ${IMAGE_SIGNATURE_LOCATION}"
}

# Sign the provided image or sign all images provided in $docker_images
if [ $# -eq 1 ]; then
  sign_image "${IMAGE}" "${TAG}"
else
  # Get context
  current_dir=$(cd "$(dirname "${0}")" && pwd)
  # shellcheck source=./scripts/ci/docker.sh
  . "${current_dir}/docker.sh"

  # Loop over images in $docker_images and use $DOCKER_IMAGE_TAG as tag
  for docker_image in ${docker_images}; do
    sign_image "${docker_image}" "${DOCKER_IMAGE_TAG}"
  done
fi

# Remove credentials
rm signer_sa.json

echo "Docker image signing process done."
