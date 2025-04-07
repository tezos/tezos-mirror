#!/bin/bash

# Sanitize tag or commit ref for docker image creation, and exports to DOCKER_IMAGE_TAG variable

if [[ "${CI_COMMIT_REF_NAME}" == "${CI_DEFAULT_BRANCH}" ]]; then
  export DOCKER_IMAGE_TAG='latest'
elif [[ -n "${CI_COMMIT_TAG:-}" ]]; then
  NOSLASH=$(echo "${CI_COMMIT_TAG}" | tr -s / -)
  SANITIZED="${NOSLASH//[^a-zA-Z0-9\-\.]/}"
  export DOCKER_IMAGE_TAG="${SANITIZED}"
else
  NOSLASH=$(echo "${CI_COMMIT_REF_NAME}" | tr -s / -)
  SANITIZED="${NOSLASH//[^a-zA-Z0-9\-]/}"
  export DOCKER_IMAGE_TAG="branch-${SANITIZED}"
fi
