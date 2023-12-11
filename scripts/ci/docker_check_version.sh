#!/bin/sh
set -eu

echo "Check that docker client version is the same than expected."

if [ "$#" -ne 1 ]; then
  echo "Usage: $(basename "$0") <expected_docker_version>" >&2
  exit 1
fi

# Get version from argument
DOCKER_VERSION=$1

# Get client version
CLIENT_VERSION=$(docker version --format '{{.Client.Version}}')

# Strip git commit hash if included
CLIENT_VERSION=${CLIENT_VERSION%%-*}

# Print versions
echo "Expected version: ${DOCKER_VERSION}"
echo "Client version: ${CLIENT_VERSION}"

# Check if the client matches expected
if [ "${CLIENT_VERSION}" = "${DOCKER_VERSION}" ]; then
  echo "Docker client version check passed!"
else
  echo "Error - Docker client version (${CLIENT_VERSION}) does not match expected (${DOCKER_VERSION})" >&2
  exit 1
fi