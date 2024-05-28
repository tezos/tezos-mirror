#!/bin/sh

./scripts/ci/docker_wait_for_daemon.sh
./scripts/ci/docker_check_version.sh "${DOCKER_VERSION}"
./scripts/ci/docker_registry_auth.sh
