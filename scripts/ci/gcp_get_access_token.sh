#!/bin/bash
#
# Get GCP Access Token (OIDC) using GCP Workload Identity Federation (WIF)
#
# Reads the following environment variables:
# - 'GCP_WORKLOAD_IDENTITY_FEDERATION_PROJECT_ID': The Google Cloud project ID where the Workload Identity Pool is configured.
# - 'GCP_WORKLOAD_IDENTITY_FEDERATION_POOL_ID': The ID of the Workload Identity Pool within the specified Google Cloud project.
# - 'GCP_ID_TOKEN': An OIDC identity token issued by a trusted identity provider (IdP), here, GitLab.com.
#
# Output:
# - 'ACCESS_TOKEN.secret': An OIDC identity token issued by GCP WIF if successfully authenticated.

set -uo pipefail # Strict mode

echo "GCP WIF access token generation..."

####################################################
# Generate the payload asking for the access token #
####################################################

FEDERATION_PAYLOAD=$(
  cat << EOF
  {
    "audience": "//iam.googleapis.com/projects/${GCP_WORKLOAD_IDENTITY_FEDERATION_PROJECT_ID}/locations/global/workloadIdentityPools/${GCP_WORKLOAD_IDENTITY_FEDERATION_POOL_ID}/providers/${GCP_WORKLOAD_IDENTITY_FEDERATION_PROVIDER_ID}",
    "grantType": "urn:ietf:params:oauth:grant-type:token-exchange",
    "requestedTokenType": "urn:ietf:params:oauth:token-type:access_token",
    "scope": "https://www.googleapis.com/auth/cloud-platform",
    "subjectTokenType": "urn:ietf:params:oauth:token-type:jwt",
    "subjectToken": "${GCP_ID_TOKEN}"
  }
EOF
)

#####################################################
# Exchange the GCP ID token against an access token #
#####################################################

ACCESS_TOKEN_RESPONSE=$(
  curl -X POST "https://sts.googleapis.com/v1/token" \
    --header "Accept: application/json" \
    --header "Content-Type: application/json" \
    --data "${FEDERATION_PAYLOAD}"
)

if [ "$(echo "${ACCESS_TOKEN_RESPONSE}" | jq -er '.access_token | length')" -eq 0 ]; then
  echo "WORKLOAD IDENTITY FEDERATION ERROR"
  echo "Error type: $(echo "${ACCESS_TOKEN_RESPONSE}" | jq '.error')"
  echo "Error description: $(echo "${ACCESS_TOKEN_RESPONSE}" | jq '.error_description')"
  exit 1
fi

echo "${ACCESS_TOKEN_RESPONSE}" | jq -er '.access_token' > ".ACCESS_TOKEN"

echo "GCP WIF access token generated: access granted."

exit 0
