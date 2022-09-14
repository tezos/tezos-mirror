#!/usr/bin/env bash

. scripts/ci/docker.env

if [ -z "${build_deps_image_name}" ]; then echo "build_deps_image_name is unset" && exit 3; fi
if [ -z "${build_deps_image_version}" ]; then echo "build_deps_image_version is unset" && exit 3; fi

cd "${CI_PROJECT_DIR}" || exit 1

# Environment variables from before_script
. ./scripts/ci/docker.env

# Build minimal, bare and debug images
./scripts/create_docker_image.sh \
  "${DOCKER_IMAGE_NAME}" \
  "${DOCKER_IMAGE_TAG}" \
  "${build_deps_image_name}" \
  "${build_deps_image_version}" \
  "${CI_COMMIT_SHORT_SHA}"

# auth gitlab or dockerhub registry
# notice the different namespace for gitlab and that we remove the `-`
# Test bare image
./scripts/ci/docker_smoke_test.sh "${DOCKER_IMAGE_NAME}bare:${DOCKER_IMAGE_TAG}" "${CI_COMMIT_SHORT_SHA}" version

# Push minimal, bare and debug images
./scripts/ci/docker_push_all.sh