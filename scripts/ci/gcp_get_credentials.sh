#!/bin/sh
#
# Get the GOOGLE_APPLICATION_CREDENTIALS using gcloud
#
set -eu

if command -v gcloud > /dev/null 2>&1; then
  # Auth signer service account
  echo "${GCP_SIGNER_SERVICE_ACCOUNT}" | base64 -d > signer_sa.json
  gcloud auth activate-service-account --key-file=signer_sa.json
  export GOOGLE_APPLICATION_CREDENTIALS=signer_sa.json
else
  echo "Gcloud missing: Cannot authenticate on GCP"
fi
