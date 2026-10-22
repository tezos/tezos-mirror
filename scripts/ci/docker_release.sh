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

options=$(getopt -o '' \
  -l runtime-image:,build-deps-image: -- "$@")
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
#
# Note the publish-before-test inversion: [create_docker_image.sh --push]
# pushes each per-arch image to the registry *during* the bake build, so the
# smoke test below runs against the already-published per-arch tag. A
# smoke-test failure therefore leaves an unsigned per-arch tag live in the
# registry. This is bounded: the user-facing multi-arch tags are only
# assembled later by [docker:merge_manifests] on success, and nothing is
# signed until [docker_sign.sh] runs -- so a consumer verifying the cosign
# signature never trusts a tag that failed the smoke test.

# On protected refs GCP_SCCACHE_BUCKET is the protected bucket, which only
# protected-registry@ may read. The sccache server runs inside the image build
# (build.Dockerfile), where the credentials activated in this job are out of
# reach, so the key is handed to the build as a BuildKit secret: decoded to a
# private temp file here, mounted for the compile step only, never written to
# a layer, and removed when this script exits. On unprotected refs the bucket
# is the unprotected one and Application Default Credentials (the node
# service account) are enough, as for every other job. See #8424.
sccache_key_file=""
if [ -n "${GCP_SCCACHE_BUCKET:-}" ] && [ "${CI_COMMIT_REF_PROTECTED:-}" = "true" ]; then
  sccache_key_file="${TMPDIR:-/tmp}/sccache_docker_sa.json"
  umask 077
  echo "${GCP_PROTECTED_SERVICE_ACCOUNT:?"Error: GCP_PROTECTED_SERVICE_ACCOUNT is empty on a protected ref."}" |
    base64 -d > "$sccache_key_file"
  umask 022
  trap 'rm -f "$sccache_key_file"' EXIT
  echo "### sccache: protected ref, passing the protected-registry@ key to the build as a secret"
fi

# --cargo-mirror is unconditional: this script only runs in the CI, where the
# crates-io mirror proxy resolves. Local builds call create_docker_image.sh
# directly and leave it off.
# Disable the quote-warning from shellcheck so that we can pass the
# optional --sccache-bucket / --sccache-key-file arguments.
# shellcheck disable=SC2046
./scripts/create_docker_image.sh \
  --push \
  --image-name "${DOCKER_IMAGE_NAME}" \
  --image-version "${DOCKER_IMAGE_TAG}" \
  --runtime-image "${runtime_image}" \
  --build-deps-image "${build_deps_image}" \
  --executables "${OCTEZ_EXECUTABLES}" \
  --commit-short-sha "${CI_COMMIT_SHORT_SHA}" \
  --cargo-mirror \
  $(
    # GCP_SCCACHE_BUCKET is defined in the GitLab CI/CD settings.
    if [ -n "${GCP_SCCACHE_BUCKET:-}" ]; then echo "--sccache-bucket ${GCP_SCCACHE_BUCKET}"; fi
    if [ -n "${sccache_key_file}" ]; then echo "--sccache-key-file ${sccache_key_file}"; fi
  )

# Test bare image. [create_docker_image.sh --push] already published it to
# the registry, so [docker run] auto-pulls it when absent locally.
REQUIRED_EXECUTABLES="$OCTEZ_EXECUTABLES" ./scripts/ci/docker_smoke_test.sh "${DOCKER_IMAGE_NAME}bare:${DOCKER_IMAGE_TAG}" "${CI_COMMIT_SHORT_SHA}" version

# Sign images
./scripts/ci/docker_sign.sh

# Verify signature
./scripts/ci/docker_verify_signature.sh

# Verify the SLSA provenance + SBOM attestations on every pushed image.
# Exits non-zero if any of them is missing or malformed.
./scripts/ci/docker_verify_attestation.sh
