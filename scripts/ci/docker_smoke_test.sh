#!/bin/sh

set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <docker-image> [expected-version-sha] version"
  echo
  echo 'This script is used to test Octez Docker images. It can run two test cases:'
  echo
  echo " - version: this test simply verifies that the Octez binaries"
  echo "   contained in the image execute when passed '--version' and that the"
  echo "   reported version is as expected"
  exit 1
fi

IMAGE=$1
shift
if [ $# != 0 ]; then
  SHA=$1
  shift
else
  SHA=$(git rev-parse --short=8 HEAD)
fi
TESTS=${*:-version}

test_version() {
  echo "Testing version of binaries in Docker image"

  binaries=$(docker run --entrypoint ls "$IMAGE" /usr/local/bin/)

  echo "Found the following executables in the Docker image:"
  echo "$binaries"

  for bin in octez-client octez-node; do
    if ! echo "$binaries" | grep -q "^${bin}$"; then
      echo "Could not find binary $bin in Docker image."
      exit 1
    else
      echo "Found binary $bin in Docker image"
    fi
  done

  for binary in $(echo "$binaries" | grep octez-); do
    printf "Checking version of %s: " "$binary"

    cmd="docker run --entrypoint $binary $IMAGE --version"
    version=$($cmd)
    node_sha_in_docker=$(echo "$version" | cut -d' ' -f 1)

    if [ "$node_sha_in_docker" != "$SHA" ]; then
      echo "NOK"
      echo
      echo "Expected the version of Docker $binary:"
      echo "  $version"
      echo "to reference commit $SHA, found $node_sha_in_docker instead,"
      echo "when executing '$cmd'"
      exit 1
    else
      echo "'$version', OK!"
    fi
  done
}

echo "Smoke testing docker image $IMAGE"

for test_case in $TESTS; do
  case $test_case in
  version)
    test_version
    ;;
  *)
    echo "Unknown test '$test_case'. Should be: version"
    ;;
  esac
done
