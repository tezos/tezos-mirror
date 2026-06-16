#!/bin/sh
#
# Retrieve a secret from GCP Secret Manager using a GCP Access Token.
#
# Usage:
#   printf '%s' ACCESS_TOKEN | ./gcp_sm_get_secret.sh SECRET_PATH
#
# Arguments:
#   stdin        - An OIDC access token issued by GCP WIF (passed via stdin to avoid
#                  exposing it in the process table).
#   SECRET_PATH  - GCP secret resource path (projects/PROJECT_ID/secrets/NAME/versions/VERSION).
#
# Output:
#   stdout: The decoded secret value (only output on success).

set -eu

readonly GCP_SM_BASE_URL="https://secretmanager.googleapis.com/v1"
readonly CURL_MAX_TIME=30
readonly CURL_RETRIES=3

##############################################
# Validate arguments                         #
##############################################

if [ "$#" -ne 1 ]; then
  echo "Usage: printf '%s' ACCESS_TOKEN | $0 \"projects/PROJECT_ID/secrets/NAME/versions/VERSION\"" >&2
  exit 2
fi

readonly GCP_SECRET_PATH="$1"

# Read access token from stdin to avoid exposing it in the process table.
ACCESS_TOKEN=$(cat)
if [ -z "${ACCESS_TOKEN}" ]; then
  echo "ERROR: ACCESS_TOKEN is empty (expected on stdin)." >&2
  exit 1
fi

# Basic format check
if ! echo "${GCP_SECRET_PATH}" | grep -qE '^projects/[^/]+/secrets/[^/]+/versions/[^/]+$'; then
  echo "ERROR: Invalid secret path format: '${GCP_SECRET_PATH}'" >&2
  echo "Expected: projects/PROJECT_ID/secrets/NAME/versions/VERSION" >&2
  exit 2
fi

echo "Getting secret from GCP Secret Manager (${GCP_SECRET_PATH})..." >&2

##############################################
# Disable debug to prevent token leaks       #
##############################################

{ set +x; } 2> /dev/null

##############################################
# Retrieve the secret via Secret Manager API #
##############################################

SECRET_RESPONSE=$(
  printf 'Authorization: Bearer %s\n' "${ACCESS_TOKEN}" |
    curl --silent --fail --show-error \
      --max-time "${CURL_MAX_TIME}" \
      --retry "${CURL_RETRIES}" \
      --request GET \
      --header "Content-Type: application/json" \
      --header @- \
      "${GCP_SM_BASE_URL}/${GCP_SECRET_PATH}:access"
) || {
  echo "ERROR: HTTP request to Secret Manager failed (curl exit code: $?)." >&2
  exit 1
}

##############################################
# Extract and decode the secret payload      #
##############################################

SECRET_PAYLOAD=$(echo "${SECRET_RESPONSE}" | jq -er '.payload.data' 2> /dev/null) || {
  ERROR_CODE=$(echo "${SECRET_RESPONSE}" | jq -r '.error.code // empty' 2> /dev/null)
  ERROR_MSG=$(echo "${SECRET_RESPONSE}" | jq -r '.error.message // empty' 2> /dev/null)

  echo "SECRET MANAGER ACCESS ERROR" >&2
  if [ -n "${ERROR_CODE}" ]; then
    echo "Error code: ${ERROR_CODE}" >&2
    echo "Error message: ${ERROR_MSG}" >&2
  else
    echo "Error: secret payload is empty or malformed." >&2
  fi
  exit 1
}

SECRET_VALUE=$(echo "${SECRET_PAYLOAD}" | base64 -d 2> /dev/null) || {
  echo "ERROR: Failed to base64-decode the secret payload." >&2
  exit 1
}

##############################################
# Output the secret to stdout                #
##############################################

echo "${SECRET_VALUE}"

echo "Secret (${GCP_SECRET_PATH}) retrieved successfully." >&2

exit 0
