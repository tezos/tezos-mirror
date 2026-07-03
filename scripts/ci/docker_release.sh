#!/usr/bin/env bash

set -eu

# Read environment variables written by 'docker_image_names.sh'
# (via docker_initialize.sh --image-names) in 'before_script'.
. scripts/ci/docker.env

# Full image references (name:tag) for the runtime and build-dependencies images
# the Octez distribution is built FROM. The distribution jobs pass them
# explicitly via --runtime-image / --build-deps-image; both are required. This
# script only runs in CI (docker.env, CI_PROJECT_DIR, DOCKER_IMAGE_NAME,
# EXECUTABLE_FILES are all CI-provided), so there is no local-invocation
# default: for local builds, see scripts/create_docker_image.sh.
runtime_image=""
build_deps_image=""
# The rust-toolchain image (L2 builder) used for with-EVM builds is passed
# explicitly by the job via --rust-toolchain-image.
rust_toolchain_image=""

options=$(getopt -o '' \
  -l runtime-image:,build-deps-image:,rust-toolchain-image: -- "$@")
eval set - "$options"
while true; do
  case "$1" in
  --runtime-image)
    shift
    runtime_image="$1"
    ;;
  --build-deps-image)
    shift
    build_deps_image="$1"
    ;;
  --rust-toolchain-image)
    shift
    rust_toolchain_image="$1"
    ;;
  --)
    shift
    break
    ;;
  *)
    echo "Unknown option: $1" >&2
    exit 1
    ;;
  esac
  shift
done

runtime_image=${runtime_image:?"--runtime-image is required"}
build_deps_image=${build_deps_image:?"--build-deps-image is required"}

cd "${CI_PROJECT_DIR}" || exit 1

EXECUTABLE_FILES=${EXECUTABLE_FILES:?"Error: environment variable EXECUTABLE_FILES is empty.
Set it to e.g. 'script-inputs/released-executables'
or to 'script-inputs/released-executables script-inputs/experimental-executables'."}

# shellcheck disable=SC2086
OCTEZ_EXECUTABLES="$(cat $EXECUTABLE_FILES)"

# Build minimal, bare and debug images

# Disable the quote-warning from shellcheck so that we can pass the
# optional --rust-toolchain-image / --sccache-bucket arguments.
# shellcheck disable=SC2046
./scripts/create_docker_image.sh \
  --image-name "${DOCKER_IMAGE_NAME}" \
  --image-version "${DOCKER_IMAGE_TAG}" \
  --runtime-image "${runtime_image}" \
  --build-deps-image "${build_deps_image}" \
  --executables "${OCTEZ_EXECUTABLES}" \
  --commit-short-sha "${CI_COMMIT_SHORT_SHA}" \
  --docker-target "${DOCKER_BUILD_TARGET}" \
  $(if [ -n "${rust_toolchain_image}" ]; then echo "--rust-toolchain-image ${rust_toolchain_image}"; fi) \
  $(
    # GCP_SCCACHE_BUCKET is defined in the GitLab CI/CD settings.
    if [ -n "${GCP_SCCACHE_BUCKET:-}" ]; then echo "--sccache-bucket ${GCP_SCCACHE_BUCKET}"; fi
  )

# auth gitlab or dockerhub registry
# notice the different namespace for gitlab and that we remove the `-`
# Test bare image
REQUIRED_EXECUTABLES="$OCTEZ_EXECUTABLES" ./scripts/ci/docker_smoke_test.sh "${DOCKER_IMAGE_NAME}bare:${DOCKER_IMAGE_TAG}" "${CI_COMMIT_SHORT_SHA}" version

# Push minimal, bare and debug images
./scripts/ci/docker_push_all.sh

# Sign images
./scripts/ci/docker_sign.sh

# Verify signature
./scripts/ci/docker_verify_signature.sh
