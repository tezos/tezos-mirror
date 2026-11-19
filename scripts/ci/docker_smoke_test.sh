#!/bin/sh

set -e

if [ -z "$1" ]; then
  echo "Usage: $0 <docker-image> [expected-version-sha] [version] [sapling]"
  echo
  echo 'This script is used to test Octez Docker images. It can run two test cases:'
  echo
  echo " - version: this test simply verifies that the Octez binaries"
  echo "   contained in the image execute when passed '--version' and that the"
  echo "   reported version is as expected"
  echo " - sapling: this test verifies that the Sapling parameters are present"
  echo "   where the binaries look for them"
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

REQUIRED_EXECUTABLES="${REQUIRED_EXECUTABLES:-"octez-client octez-node"}"

test_version() {
  echo "Testing version of binaries in Docker image"

  binaries=$(docker run --entrypoint ls "$IMAGE" /usr/local/bin/)

  echo "Found the following executables in the Docker image:"
  echo "$binaries"

  for bin in $REQUIRED_EXECUTABLES; do
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

# The released binaries find the Sapling parameters in /usr/share/zcash-params
# (the last directory tried by [find_params] in src/lib_sapling/rustzcash.ml).
# Only images/ci/Dockerfile.runtime puts them there, and nothing else checks it:
# a distribution image has no opam switch and no source tree, so the other
# candidate directories are empty. [octez-node run] then fails at startup
# (see [init_zcash] in src/bin_node/node_run_command.ml), but [--version] does
# not touch the parameters, so [test_version] cannot catch a missing copy.
test_sapling() {
  echo "Testing Sapling parameters in Docker image"

  for params in sapling-spend.params sapling-output.params; do
    if docker run --entrypoint ls "$IMAGE" "/usr/share/zcash-params/$params" \
      > /dev/null 2>&1; then
      echo "Found /usr/share/zcash-params/$params"
    else
      echo "Missing /usr/share/zcash-params/$params in Docker image."
      echo "Octez binaries would fail to start; see images/ci/Dockerfile.runtime."
      exit 1
    fi
  done
}

echo "Smoke testing docker image $IMAGE"

for test_case in $TESTS; do
  case $test_case in
  version)
    test_version
    ;;
  sapling)
    test_sapling
    ;;
  *)
    echo "Unknown test '$test_case'. Should be: version or sapling"
    ;;
  esac
done
