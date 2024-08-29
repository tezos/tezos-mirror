#!/usr/bin/env bash

set -eu

# Read environment variables written by 'docker_registry_auth.sh' in
# 'before_script'.
. scripts/ci/docker.env

# Note: 'ci_image_name' is set in the variables section
# of the top-level .gitlab-ci.yml.
ci_image_name=${ci_image_name:?"ci_image_name: is unset"}
# Note: 'ci_image_tag' is dynamically set by an incoming
# dotenv file from the job that produces the CI images
# (currently: 'oc.docker:ci:*')
ci_image_version=${ci_image_tag:?"ci_image_tag: is unset"}

cd "${CI_PROJECT_DIR}" || exit 1

EXECUTABLE_FILES=${EXECUTABLE_FILES:?"Error: environment variable EXECUTABLE_FILES is empty.
Set it to e.g. 'script-inputs/released-executables'
or to 'script-inputs/released-executables script-inputs/experimental-executables'."}

# shellcheck disable=SC2086
OCTEZ_EXECUTABLES="$(cat $EXECUTABLE_FILES)"

# Build minimal, bare and debug images

# Disable the quote-warning from shellcheck so that we can pass the
# optional arguments rust_toolchain_image_{name,tag}
# shellcheck disable=SC2046
./scripts/create_docker_image.sh \
  --image-name "${DOCKER_IMAGE_NAME}" \
  --image-version "${DOCKER_IMAGE_TAG}" \
  --ci-image-name "${ci_image_name}" \
  --ci-image-version "${ci_image_version}" \
  --executables "${OCTEZ_EXECUTABLES}" \
  --commit-short-sha "${CI_COMMIT_SHORT_SHA}" \
  --docker-target "${DOCKER_BUILD_TARGET}" \
  $(if [ -n "${rust_toolchain_image_name:-}" ]; then echo "--rust-toolchain-image-name ${rust_toolchain_image_name}"; fi) \
  $(if [ -n "${rust_toolchain_image_tag:-}" ]; then echo "--rust-toolchain-image-tag ${rust_toolchain_image_tag}"; fi)

# auth gitlab or dockerhub registry
# notice the different namespace for gitlab and that we remove the `-`
# Test bare image
REQUIRED_EXECUTABLES="$OCTEZ_EXECUTABLES" ./scripts/ci/docker_smoke_test.sh "${DOCKER_IMAGE_NAME}bare:${DOCKER_IMAGE_TAG}" "${CI_COMMIT_SHORT_SHA}" version

# Push minimal, bare and debug images
./scripts/ci/docker_push_all.sh

# Sign image signatures
./scripts/ci/docker_sign.sh
