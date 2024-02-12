#!/usr/bin/env bash

set -e

. scripts/ci/docker.env

if [ -z "${build_deps_image_name}" ]; then echo "build_deps_image_name is unset" && exit 3; fi
if [ -z "${build_deps_image_version}" ]; then echo "build_deps_image_version is unset" && exit 3; fi
if [ -z "${rust_toolchain_image_name}" ]; then echo "rust_toolchain_image_name is unset" && exit 3; fi
if [ -z "${rust_toolchain_image_version}" ]; then echo "rust_toolchain_image_version is unset" && exit 3; fi

cd "${CI_PROJECT_DIR}" || exit 1

# Environment variables from before_script
. ./scripts/ci/docker.env

if [ -z "$EXECUTABLE_FILES" ]; then
  echo "Error: environment variable EXECUTABLE_FILES is empty."
  echo "Set it to e.g. 'script-inputs/released-executables'"
  echo "or to 'script-inputs/released-executables script-inputs/experimental-executables'."
  exit 1
fi

# shellcheck disable=SC2086
OCTEZ_EXECUTABLES="$(cat $EXECUTABLE_FILES)"

# Build minimal, bare and debug images
./scripts/create_docker_image.sh \
  "${DOCKER_IMAGE_NAME}" \
  "${DOCKER_IMAGE_TAG}" \
  "${build_deps_image_name}" \
  "${build_deps_image_version}" \
  "${OCTEZ_EXECUTABLES}" \
  "${CI_COMMIT_SHORT_SHA}" \
  "${DOCKER_BUILD_TARGET}" \
  "${rust_toolchain_image_name}" \
  "${rust_toolchain_image_version}"

# auth gitlab or dockerhub registry
# notice the different namespace for gitlab and that we remove the `-`
# Test bare image
./scripts/ci/docker_smoke_test.sh "${DOCKER_IMAGE_NAME}bare:${DOCKER_IMAGE_TAG}" "${CI_COMMIT_SHORT_SHA}" version

# Push minimal, bare and debug images
./scripts/ci/docker_push_all.sh
