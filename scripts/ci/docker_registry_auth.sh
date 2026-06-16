#!/bin/sh
set -eu

# Setup Docker registry authentication for either Docker Hub (release)
# or GCP Artefact registry (dev).

# Export Google Auth token to build DockerFile
echo "Current active user: $(gcloud config get-value account)"

# create secret for npm registry
gcloud auth print-access-token > /tmp/npm_token.txt

if [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then
  echo "### Logging into protected repo ..."
  echo "${GCP_PROTECTED_SERVICE_ACCOUNT}" | base64 -d > protected_sa.json
  gcloud auth activate-service-account --key-file=protected_sa.json
else
  echo "### Logging into standard repo ..."
  # Nothing to do
fi

gcloud auth configure-docker us.gcr.io
gcloud auth print-access-token | docker login -u oauth2accesstoken --password-stdin https://us-central1-docker.pkg.dev

# CI_DOCKER_HUB is used to switch to Docker Hub if credentials
# are available with CI_DOCKER_AUTH
# CI_DOCKER_AUTH is only available on protected branches
if [ "${CI_DOCKER_HUB:-}" = 'true' ] && [ "${CI_PROJECT_NAMESPACE}" = "tezos" ] && [ -n "${CI_DOCKER_AUTH:-}" ]; then

  # Create directory for Docker JSON configuration (if does not exist)
  mkdir -pv ~/.docker

  # Docker Hub
  echo "### Logging into Docker Hub for pushing images"

  # Publishing to Docker Hub, as done on protected refs, has higher priority.
  echo "{\"auths\":{\"https://index.docker.io/v1/\":{\"auth\":\"${CI_DOCKER_AUTH}\"}}}" > ~/.docker/config.json
fi

# Call a helper script to setup docker image names and tags
scripts/ci/docker_image_names.sh
