#!/bin/bash
#
# Display secret stored in GCP Secret Manager using a GCP Access Token
#
# Usage:
# - '$1': secret var name used later in GitLab (YOURSECRET)
# - '$2': GCP secret path that must match GCP secret resource name and version (projects/GCP_PROJECT_ID/secrets/SECRET_NAME/versions/SECRET_VERSION)
#
# Reads the following environment variables:
# - 'ACCESS_TOKEN': An OIDC identity token issued by GCP WIF if successfully authenticated.
#
# Output:
# - YOURSECRET.secret: The secret value retrieved from GCP will be written in text files

set -uo pipefail # Strict mode

if [[ "$#" -ne 2 ]]; then
  echo "Usage: $0 YOURSECRET \"projects/GCP_PROJECT_ID/secrets/YOURSECRET_NAME/versions/YOURSECRET_VERSION\""
  exit 2
fi

echo "Getting secret stored in GCP Secret Manager..."

###########################################
# Get the secret value using access token #
###########################################

SECRET_VAR="$1"
GCP_SECRET_PATH="$2"

ACCESS_TOKEN=$(cat .ACCESS_TOKEN)

SECRET_RESPONSE=$(
  curl "https://secretmanager.googleapis.com/v1/${GCP_SECRET_PATH}:access" \
    --header "content-type: application/json" \
    --request "GET" \
    --header "authorization: Bearer ${ACCESS_TOKEN}"
)

if [ "$(echo "${SECRET_RESPONSE}" | jq -er '.payload.data | length')" -eq 0 ]; then
  echo "SECRET MANAGER ACCESS ERROR"
  if [ "$(echo "${SECRET_RESPONSE}" | jq -er '.error.code | length')" -ne 0 ]; then
    echo "Error code: $(echo "${SECRET_RESPONSE}" | jq '.error.code')"
    echo "Error message: $(echo "${SECRET_RESPONSE}" | jq '.error.message')"
  else
    echo "Error ${SECRET_VAR}'s payload is empty."
  fi
  exit 1
fi

echo "${SECRET_RESPONSE}" | jq -er '.payload.data' | base64 -d > "${SECRET_VAR}.secret"

echo "Secret ${SECRET_VAR} ($GCP_SECRET_PATH) ready to use!"

exit 0
