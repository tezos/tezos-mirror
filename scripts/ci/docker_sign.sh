#!/bin/sh
#
# Sign the releasing docker images using Cosign
#
# Reads the following environment variables:
#  - 'GCP_SIGNER_SERVICE_ACCOUNT': signer service account key file encoded
#     in base64, set by GitLab CI
#  - 'GCP_SIGN_KEY': key used for signing, format:
#     `gcpkms://projects/<GCP_PROJECT_CONTAINING_THE_KEY_PAIR>/locations/<LOCATION>/keyRings/<KEYRING_NAME>/cryptoKeys/<KEY_NAME>/versions/<KEY_VERSION>`,
#     set by GitLab CI

set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

# Auth signer service account
echo "${GCP_SIGNER_SERVICE_ACCOUNT}" | base64 -d > signer_sa.json
gcloud auth activate-service-account --key-file=signer_sa.json
gcloud auth configure-docker us-central1-docker.pkg.dev
gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin https://us-central1-docker.pkg.dev
export GOOGLE_APPLICATION_CREDENTIALS=signer_sa.json

# Install cosign
apk add --update cosign
cosign version

# Loop over images
for docker_image in ${docker_images}; do

  # Get image digest
  IMAGE_DIGEST="${docker_image}@$(docker buildx imagetools inspect "${docker_image}:${DOCKER_IMAGE_TAG}" --format "{{json .Manifest}}" | jq -r '.digest')"
  echo "Image digest: ${IMAGE_DIGEST}"

  # Sign image with cosign
  cosign sign --key "${GCP_SIGN_KEY}" "${IMAGE_DIGEST}" -y

  # Get the location of image signature as reference
  IMAGE_SIGNATURE_LOCATION=$(cosign triangulate "${IMAGE_DIGEST}")
  echo "Image signature location: ${IMAGE_SIGNATURE_LOCATION}"
done

# Remove credentials
rm signer_sa.json
