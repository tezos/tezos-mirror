#!/bin/sh

# Initialize Docker environment for CI jobs.
#
# Usage:
#   ./scripts/ci/docker_initialize.sh                # auth only
#   ./scripts/ci/docker_initialize.sh --image-names  # auth + compute image names
#
# Without --image-names: sets up Docker daemon and registry authentication.
# Suitable for CI image builds, base image builds, rust toolchain builds,
# and packaging/systemd test jobs.
#
# With --image-names: additionally computes Octez distribution image names
# (DOCKER_IMAGE_NAME, DOCKER_IMAGE_TAG) via docker_image_names.sh.
# Required by Octez distribution jobs (docker_release.sh, docker_push_all.sh,
# docker_merge_manifests.sh, docker_sign.sh, docker_verify_signature.sh).

./scripts/ci/docker_wait_for_daemon.sh
./scripts/ci/docker_check_version.sh "${DOCKER_VERSION}"
./scripts/ci/docker_registry_auth.sh

if [ "${1:-}" = "--image-names" ]; then
  ./scripts/ci/docker_image_names.sh
fi
