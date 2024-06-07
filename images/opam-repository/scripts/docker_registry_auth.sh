#!/bin/sh
set -eu

# Docker JSON configuration to access registries

mkdir -pv /root/.docker
CI_REGISTRY_AUTH=$(printf '%s:%s' "${CI_REGISTRY_USER}" "${CI_REGISTRY_PASSWORD}" | base64 | tr -d '\n')

if [ -n "${AWS_ECR:-}" ]; then
  # GitLab container registry + AWS ECR (for mirroring)
  echo "{\"auths\":{\"registry.gitlab.com\":{\"auth\":\"${CI_REGISTRY_AUTH}\"}},\"credHelpers\":{\"${AWS_ECR}\":\"ecr-login\"}}" > /root/.docker/config.json
else
  # GitLab container registry only
  echo "{\"auths\":{\"registry.gitlab.com\":{\"auth\":\"${CI_REGISTRY_AUTH}\"}}}" > /root/.docker/config.json
fi

if [ -n "${GCP_REGISTRY:-}" ]; then
  # add GCP Artifact Registry
  GCP_ARTIFACT_REGISTRY_TOKEN=$(curl -s -H "Metadata-Flavor: Google" http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token | cut -d'"' -f4)
  echo "${GCP_ARTIFACT_REGISTRY_TOKEN}" | docker login us-central1-docker.pkg.dev -u oauth2accesstoken --password-stdin
fi
