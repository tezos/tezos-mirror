#!/bin/sh
set -eu

# Docker JSON configuration to access registries

mkdir -pv /root/.docker
CI_REGISTRY_AUTH=$(printf '%s:%s' "${CI_REGISTRY_USER}" "${CI_REGISTRY_PASSWORD}" | base64 | tr -d '\n')

echo "{\"auths\":{\"registry.gitlab.com\":{\"auth\":\"${CI_REGISTRY_AUTH}\"}}}" > /root/.docker/config.json

# GCP auth with standard registry
if [ -n "${GCP_REGISTRY:-}" ]; then
  echo "Authentication to GCP with standard Service Account ..."
  GCP_ARTIFACT_REGISTRY_TOKEN=$(curl -s -H "Metadata-Flavor: Google" http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token | cut -d'"' -f4)
  echo "${GCP_ARTIFACT_REGISTRY_TOKEN}" | docker login us-central1-docker.pkg.dev -u oauth2accesstoken --password-stdin
fi

if [ "${CI_COMMIT_REF_PROTECTED:-false}" = true ]; then

  # GCP auth with protected registry
	echo "Authentication to GCP with protected Service Account ..."

	echo ${GCP_PROTECTED_SERVICE_ACCOUNT} | base64 -d > protected_sa.json

	# Extract the private key and client email from the service account JSON key file
	PRIVATE_KEY=$(jq -r '.private_key' protected_sa.json)
	CLIENT_EMAIL=$(jq -r '.client_email' protected_sa.json)

	# Create a JWT header
	JWT_HEADER=$(echo -n '{"alg":"RS256","typ":"JWT"}' | base64 | tr -d '\n=')

	# Create a JWT claim set
	JWT_CLAIM_SET=$(echo -n '{"iss":"'"${CLIENT_EMAIL}"'","scope":"https://www.googleapis.com/auth/cloud-platform","aud":"https://oauth2.googleapis.com/token","exp":'$(($(date +%s)+3600))',"iat":'$(date +%s)'}' | base64 | tr -d '\n=')

	# Create a JWT signature
	JWT_SIGNATURE=$(echo -n "${JWT_HEADER}.${JWT_CLAIM_SET}" | openssl dgst -sha256 -sign <(echo -n "${PRIVATE_KEY}") -binary | base64 -w 0 | tr '+/' '-_' | tr -d '=')

	# Create a JWT
	JWT="${JWT_HEADER}.${JWT_CLAIM_SET}.${JWT_SIGNATURE}"

	# Request an access token
	ACCESS_TOKEN=$(curl -s -X POST -H "Content-Type: application/x-www-form-urlencoded" -d "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=${JWT}" https://oauth2.googleapis.com/token | jq -r '.access_token')

	# Use the access token to authenticate with Docker
	echo $ACCESS_TOKEN | docker login us-central1-docker.pkg.dev -u oauth2accesstoken --password-stdin
fi
